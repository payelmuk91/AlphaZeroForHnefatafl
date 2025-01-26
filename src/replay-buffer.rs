use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use rand::prelude::*;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use tch::{Device, Tensor};
use crate::models::MuZeroNetwork;

#[derive(Clone, Serialize, Deserialize)]
struct Config {
    seed: u64,
    PER: bool,
    PER_alpha: f32,
    replay_buffer_size: usize,
    batch_size: usize,
    stacked_observations: usize,
    action_space: Vec<i32>,
    num_unroll_steps: usize,
    td_steps: usize,
    discount: f32,
    training_steps: usize,
    reanalyse_on_gpu: bool,
    use_last_model_value: bool,
    support_size: i32,
}

#[derive(Clone, Serialize, Deserialize)]
struct GameHistory {
    root_values: Vec<f32>,
    priorities: Option<Vec<f32>>,
    game_priority: f32,
    reward_history: Vec<f32>,
    to_play_history: Vec<i32>,
    action_history: Vec<i32>,
    child_visits: Vec<Vec<f32>>,
    reanalysed_predicted_root_values: Option<Vec<f32>>,
}

struct ReplayBuffer {
    config: Config,
    buffer: HashMap<usize, GameHistory>,
    num_played_games: usize,
    num_played_steps: usize,
    total_samples: usize,
}

impl ReplayBuffer {
    fn new(initial_checkpoint: &HashMap<String, usize>, initial_buffer: HashMap<usize, GameHistory>, config: Config) -> Self {
        let num_played_games = initial_checkpoint["num_played_games"];
        let num_played_steps = initial_checkpoint["num_played_steps"];
        let total_samples = initial_buffer.values().map(|gh| gh.root_values.len()).sum();

        if total_samples != 0 {
            println!("Replay buffer initialized with {} samples ({} games).\n", total_samples, num_played_games);
        }

        let mut rng = rand::thread_rng();
        rng.seed_from_u64(config.seed);

        ReplayBuffer {
            config,
            buffer: initial_buffer,
            num_played_games,
            num_played_steps,
            total_samples,
        }
    }

    fn save_game(&mut self, game_history: GameHistory, shared_storage: Option<Arc<Mutex<SharedStorage>>>) {
        if self.config.PER {
            if let Some(priorities) = &game_history.priorities {
                let mut priorities = priorities.clone();
            } else {
                let priorities: Vec<f32> = game_history.root_values.iter().enumerate().map(|(i, &root_value)| {
                    (root_value - self.compute_target_value(&game_history, i)).abs().powf(self.config.PER_alpha)
                }).collect();
                game_history.priorities = Some(priorities.clone());
                game_history.game_priority = *priorities.iter().max_by(|a, b| a.partial_cmp(b).unwrap()).unwrap();
            }
        }

        self.buffer.insert(self.num_played_games, game_history.clone());
        self.num_played_games += 1;
        self.num_played_steps += game_history.root_values.len();
        self.total_samples += game_history.root_values.len();

        if self.config.replay_buffer_size < self.buffer.len() {
            let del_id = self.num_played_games - self.buffer.len();
            self.total_samples -= self.buffer[&del_id].root_values.len();
            self.buffer.remove(&del_id);
        }

        if let Some(shared_storage) = shared_storage {
            let mut storage = shared_storage.lock().unwrap();
            storage.set_info("num_played_games", self.num_played_games);
            storage.set_info("num_played_steps", self.num_played_steps);
        }
    }

    fn get_buffer(&self) -> &HashMap<usize, GameHistory> {
        &self.buffer
    }

    fn get_batch(&self) -> (Vec<(usize, usize)>, (Vec<Tensor>, Vec<Vec<i32>>, Vec<Vec<f32>>, Vec<Vec<f32>>, Vec<Vec<Vec<f32>>>, Option<Vec<f32>>, Vec<Vec<usize>>)) {
        let mut index_batch = vec![];
        let mut observation_batch = vec![];
        let mut action_batch = vec![];
        let mut reward_batch = vec![];
        let mut value_batch = vec![];
        let mut policy_batch = vec![];
        let mut gradient_scale_batch = vec![];
        let mut weight_batch = if self.config.PER { Some(vec![]) } else { None };

        for (game_id, game_history, game_prob) in self.sample_n_games(self.config.batch_size) {
            let (game_pos, pos_prob) = self.sample_position(&game_history);
            let (values, rewards, policies, actions) = self.make_target(&game_history, game_pos);

            index_batch.push((game_id, game_pos));
            observation_batch.push(game_history.get_stacked_observations(game_pos, self.config.stacked_observations, self.config.action_space.len()));
            action_batch.push(actions);
            value_batch.push(values);
            reward_batch.push(rewards);
            policy_batch.push(policies);
            gradient_scale_batch.push(vec![self.config.num_unroll_steps.min(game_history.action_history.len() - game_pos); actions.len()]);

            if let Some(weight_batch) = &mut weight_batch {
                weight_batch.push(1.0 / (self.total_samples as f32 * game_prob * pos_prob));
            }
        }

        if let Some(weight_batch) = &mut weight_batch {
            let max_weight = weight_batch.iter().cloned().fold(f32::NEG_INFINITY, f32::max);
            for weight in weight_batch.iter_mut() {
                *weight /= max_weight;
            }
        }

        (index_batch, (observation_batch, action_batch, value_batch, reward_batch, policy_batch, weight_batch, gradient_scale_batch))
    }

    fn sample_game(&self, force_uniform: bool) -> (usize, &GameHistory, f32) {
        let game_prob;
        let game_index;

        if self.config.PER && !force_uniform {
            let game_probs: Vec<f32> = self.buffer.values().map(|gh| gh.game_priority).collect();
            let sum_probs: f32 = game_probs.iter().sum();
            let game_probs: Vec<f32> = game_probs.iter().map(|&p| p / sum_probs).collect();
            game_index = game_probs.iter().enumerate().map(|(i, &p)| (i, p)).choose_weighted(&mut rand::thread_rng(), |item| item.1).unwrap().0;
            game_prob = game_probs[game_index];
        } else {
            game_index = rand::thread_rng().gen_range(0..self.buffer.len());
            game_prob = 1.0 / self.buffer.len() as f32;
        }

        let game_id = self.num_played_games - self.buffer.len() + game_index;
        (game_id, &self.buffer[&game_id], game_prob)
    }

    fn sample_n_games(&self, n_games: usize) -> Vec<(usize, &GameHistory, f32)> {
        let mut selected_games = vec![];
        let mut game_prob_dict = HashMap::new();

        if self.config.PER {
            let game_probs: Vec<f32> = self.buffer.values().map(|gh| gh.game_priority).collect();
            let sum_probs: f32 = game_probs.iter().sum();
            let game_probs: Vec<f32> = game_probs.iter().map(|&p| p / sum_probs).collect();
            let game_ids: Vec<usize> = self.buffer.keys().cloned().collect();
            let selected_game_ids: Vec<usize> = game_ids.choose_multiple_weighted(&mut rand::thread_rng(), n_games, |&id| game_probs[id]).unwrap().cloned().collect();

            for &game_id in &selected_game_ids {
                game_prob_dict.insert(game_id, game_probs[game_id]);
            }

            selected_games = selected_game_ids.iter().map(|&id| (id, &self.buffer[&id], game_prob_dict[&id])).collect();
        } else {
            let game_ids: Vec<usize> = self.buffer.keys().cloned().collect();
            selected_games = game_ids.choose_multiple(&mut rand::thread_rng(), n_games).unwrap().iter().map(|&&id| (id, &self.buffer[&id], 1.0 / self.buffer.len() as f32)).collect();
        }

        selected_games
    }

    fn sample_position(&self, game_history: &GameHistory) -> (usize, f32) {
        let position_prob;
        let position_index;

        if self.config.PER {
            let position_probs: Vec<f32> = game_history.priorities.as_ref().unwrap().iter().map(|&p| p / game_history.priorities.as_ref().unwrap().iter().sum::<f32>()).collect();
            position_index = position_probs.iter().enumerate().map(|(i, &p)| (i, p)).choose_weighted(&mut rand::thread_rng(), |item| item.1).unwrap().0;
            position_prob = position_probs[position_index];
        } else {
            position_index = rand::thread_rng().gen_range(0..game_history.root_values.len());
            position_prob = 1.0 / game_history.root_values.len() as f32;
        }

        (position_index, position_prob)
    }
    fn update_game_history(&mut self, game_id: usize, game_history: GameHistory) {
        if self.buffer.contains_key(&game_id) {
            if self.config.PER {
                let mut priorities = game_history.priorities.clone().unwrap();
            }
            self.buffer.insert(game_id, game_history);
        }
    }

    fn update_priorities(&mut self, priorities: Vec<Vec<f32>>, index_info: Vec<(usize, usize)>) {
        for (i, &(game_id, game_pos)) in index_info.iter().enumerate() {
            if self.buffer.contains_key(&game_id) {
                let priority = &priorities[i];
                let start_index = game_pos;
                let end_index = (game_pos + priority.len()).min(self.buffer[&game_id].priorities.as_ref().unwrap().len());
                self.buffer.get_mut(&game_id).unwrap().priorities.as_mut().unwrap()[start_index..end_index].copy_from_slice(&priority[..end_index - start_index]);
                self.buffer.get_mut(&game_id).unwrap().game_priority = *self.buffer[&game_id].priorities.as_ref().unwrap().iter().max_by(|a, b| a.partial_cmp(b).unwrap()).unwrap();
            }
        }
    }

    fn compute_target_value(&self, game_history: &GameHistory, index: usize) -> f32 {
        let bootstrap_index = index + self.config.td_steps;
        let mut value = 0.0;

        if bootstrap_index < game_history.root_values.len() {
            let root_values = if let Some(reanalysed_values) = &game_history.reanalysed_predicted_root_values {
                reanalysed_values
            } else {
                &game_history.root_values
            };

            let last_step_value = if game_history.to_play_history[bootstrap_index] == game_history.to_play_history[index] {
                root_values[bootstrap_index]
            } else {
                -root_values[bootstrap_index]
            };

            value = last_step_value * self.config.discount.powi(self.config.td_steps as i32);
        }

        for (i, &reward) in game_history.reward_history[index + 1..bootstrap_index + 1].iter().enumerate() {
            value += if game_history.to_play_history[index] == game_history.to_play_history[index + i] {
                reward
            } else {
                -reward
            } * self.config.discount.powi(i as i32);
        }

        value
    }

    fn make_target(&self, game_history: &GameHistory, state_index: usize) -> (Vec<f32>, Vec<f32>, Vec<Vec<f32>>, Vec<i32>) {
        let mut target_values = vec![];
        let mut target_rewards = vec![];
        let mut target_policies = vec![];
        let mut actions = vec![];

        for current_index in state_index..=state_index + self.config.num_unroll_steps {
            let value = self.compute_target_value(game_history, current_index);

            if current_index < game_history.root_values.len() {
                target_values.push(value);
                target_rewards.push(game_history.reward_history[current_index]);
                target_policies.push(game_history.child_visits[current_index].clone());
                actions.push(game_history.action_history[current_index]);
            } else if current_index == game_history.root_values.len() {
                target_values.push(0.0);
                target_rewards.push(game_history.reward_history[current_index]);
                target_policies.push(vec![1.0 / game_history.child_visits[0].len() as f32; game_history.child_visits[0].len()]);
                actions.push(game_history.action_history[current_index]);
            } else {
                target_values.push(0.0);
                target_rewards.push(0.0);
                target_policies.push(vec![1.0 / game_history.child_visits[0].len() as f32; game_history.child_visits[0].len()]);
                actions.push(rand::thread_rng().gen_range(0..self.config.action_space.len()) as i32);
            }
        }

        (target_values, target_rewards, target_policies, actions)
    }
}

struct SharedStorage {
    info: HashMap<String, usize>,
}

impl SharedStorage {
    fn new() -> Self {
        SharedStorage {
            info: HashMap::new(),
        }
    }

    fn set_info(&mut self, key: &str, value: usize) {
        self.info.insert(key.to_string(), value);
    }

    fn get_info(&self, key: &str) -> usize {
        *self.info.get(key).unwrap_or(&0)
    }
}

struct Reanalyse {
    config: Config,
    model: MuZeroNetwork,
    num_reanalysed_games: usize,
}

impl Reanalyse {
    fn new(initial_checkpoint: &HashMap<String, usize>, config: Config) -> Self {
        let mut rng = rand::thread_rng();
        rng.seed_from_u64(config.seed);
        tch::manual_seed(config.seed as i64);

        let mut model = MuZeroNetwork::new(&config);
        model.set_weights(&initial_checkpoint["weights"]);
        model.to(Device::CudaIf(config.reanalyse_on_gpu));
        model.eval();

        Reanalyse {
            config,
            model,
            num_reanalysed_games: initial_checkpoint["num_reanalysed_games"],
        }
    }

    fn reanalyse(&mut self, replay_buffer: Arc<Mutex<ReplayBuffer>>, shared_storage: Arc<Mutex<SharedStorage>>) {
        while shared_storage.lock().unwrap().get_info("num_played_games") < 1 {
            std::thread::sleep(std::time::Duration::from_millis(100));
        }

        while shared_storage.lock().unwrap().get_info("training_step") < self.config.training_steps && !shared_storage.lock().unwrap().get_info("terminate") {
            self.model.set_weights(&shared_storage.lock().unwrap().get_info("weights"));

            let (game_id, game_history, _) = replay_buffer.lock().unwrap().sample_game(true);

            if self.config.use_last_model_value {
                let observations: Vec<Tensor> = (0..game_history.root_values.len()).map(|i| {
                    Tensor::of_slice(&game_history.get_stacked_observations(i, self.config.stacked_observations, self.config.action_space.len())).to_device(Device::CudaIf(self.config.reanalyse_on_gpu))
                }).collect();

                let values = self.model.initial_inference(&observations).0;
                let values = values.squeeze().to_device(Device::Cpu).into();

                game_history.reanalysed_predicted_root_values = Some(values);
                replay_buffer.lock().unwrap().update_game_history(game_id, game_history);
            }

            self.num_reanalysed_games += 1;
            shared_storage.lock().unwrap().set_info("num_reanalysed_games", self.num_reanalysed_games);
        }
    }
}

fn main() {
    println!("Hello, world!");
    // Add your program's entry point logic here
}
