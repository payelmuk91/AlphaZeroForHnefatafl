use std::collections::HashMap;

use rand::prelude::*;


// MCTS implementation. The code below is far from complete, some missing functions, assume the existence of game code, etc..
// I might drastically change this, especially the Node struct (whose change would propagate to the whole thing)
#[derive(Debug, Clone)]
struct Node<Action, State> {
    state: State, // Replace with your actual game state type
    parent: Option<(Node<Action, State>, Action)>,
    children: HashMap<Action, Box<Node<Action, State>>>, // Replace with the actual action type // We want to record not only the child nodes but also how to get to that node
    visits: f64,
    valid_actions: Vec<Action>, // I have a feeling that we don't need this
    action_probs: HashMap<Action, f64>,
    action_counts: HashMap<Action, f64>, // maybe we don't need this cause it's a tree?
    action_Qs: HashMap<Action, f64>,



    // // (s,a) (=> s') if we regard (s,a) as the next state s', then no need to assign action_probs/Qs?
    // // Idk if it's ultimately better.
    // update: I think whichever is fine, as long as I stick to my choice. I will go with using hashmaps for probs, Qs, counts...
    // IMPORTANT: using Action as the key type is most likely unwise, for nodes other than the root we don't need to have complete knowledge of the actions,
    // so I think I should just use more suitable (like u32) type for key.
    // prob: f64,
    // Q_value: f64,
}

impl<Action: Eq + std::hash::Hash, State> Node<Action, State> {
    fn new(state: State,
           parent: Option<Node<Action, State>>,
           valid_actions: Vec<Action>,
           visits: f64) -> Self {
        Node {
            state,
            parent,
            children: HashMap::new(),
            valid_actions,
            action_probs: HashMap::new(),
            action_counts: HashMap::new(),
            action_Qs: HashMap::new(),

            // prob: 
            // visits: 0.0,
        }
    }

    fn uct_value(&self, action: Action) -> f64 {
        if self.action_counts(action) == 0.0 {
            return f64::INFINITY;
        }
        // To do; add some dirichlet noise
        self.action_Qs(action) + c_puct * self.action_probs(action) * self.visits.sqrt() / (1 + self.action_counts(action));
    }

    // fn uct_value(&self) -> f64 {
    //     if self.visits == 0.0 {
    //         return f64::INFINITY;
    //     }
    //     let parent_visit = match self.parent {
    //         Some(parent) => parent.visits ,
    //         None(_) => 0.0 // Just a placeholder value. This case need not be considered. 
    //     }
    //     // .....
    // }
}

struct Tree {
    nodes: HashSet<State>, // replace with the actual type for the game states
    root: Node,
}

impl Tree {
    fn new(root_state: String) -> Self {
        let mut nodes = HashMap::new();
        let root = Node::new(root_state, None);
        nodes.insert(0, root);
        Tree { nodes, root: 0 }
    }

    // Cannot trust. 
    fn select(&self) -> usize {
        let mut node_id = self.root;
        while !self.nodes[&node_id].children.is_empty() {
            node_id = *self.nodes[&node_id]
                .children
                .iter()
                .max_by(|&&a, &&b| {
                    self.nodes[&a]
                        .uct_value(total_visits)
                        .partial_cmp(&self.nodes[&b].uct_value(total_visits))
                        .unwrap()
                })
                .unwrap();
        }
        node_id
    }
}

// performs MCTS search until a new node is found. String should be replaced with the actual type for the actions
fn search(&self) -> (Node, String) {
    let mut node = &mut self.root;
    while true {
        let next_action = node.valid_actions
            .iter()
            .max_by(|a, b| {
                node.uct_value(a).partial_cmp(node.uct_value(b)).unwrap()
            })
            .unwrap();
        
        let next_state = node.do_move(next_action);

        if game.Endstate(next_state) {
            let reward = game.reward(next_state);
            expand(node, next_state);
            backpropagate(node, reward);
            break;
        }
        // If the new_state is unreached
        if !node.children.contains_key(next_action) {
            let (prob, reward) = nn.pred(next_state);
            expand(node, action, next_state);
            backpropagate(node, reward);
            break;
        }
        node = node.children.get(&next_action).unwrap();
    }
} 

// fn expand(&mut self, node_id: usize, new_state: String) -> usize {
//     let new_node_id = self.nodes.len();
//     let new_node = Node::new(new_state, Some(node_id));
//     self.nodes.get_mut(&node_id).unwrap().children.push(new_node_id);
//     self.nodes.insert(new_node_id, new_node);
//     new_node_id
// }

fn expand(parent: &mut Node<Action, State>, action: &Action, next_state: &State) {
    let (prob, reward) = nnmodel.predict(next_state);
    let valid_actions = game.get_valid_actions(next_state); // We don't need this?
    let valid_actions_for_masking = game.get_valid_actions_for_masking(next_state); //Ideally this should be a vector of {0,1}.
    let mut actions_probs = prob * valid_actions_for_masking; // extract only the valid actions. Todo: make this operation element-wise
    let sum_prob = actions_probs.sum();
    if sum_prob > 0 {
        actions_prob /= sum_prob;                           // renormalize
    } else {                                                // Contingency for when all the actions with non-zero probabilities are masked
        let num_valid_actions = valid_actions_for_masking.sum();
        actions_probs = valid_actions_for_masking / num_valid_actions;
    }

    let mut new_node: Node<Action, State> = Node{
        state: next_state,
        parent: parent,
        children: HashMap::new(),
        visits: 1.0,
        valid_actions: valid_actions,
        actions_probs: actions_prob,
        actions_counts: HashMap::new(),
        actions_Qs: HashMap::new(),
    };
    parent.children.insert(action, Box::new(new_node));
}

// fn backpropagate(&mut self, node_id: usize, result: f64) {
//     let mut current = Some(node_id);
//     while let Some(id) = current {
//         let node = self.nodes.get_mut(&id).unwrap();
//         node.visits += 1.0;
//         node.wins += result;
//         current = node.parent;
//     }
// }

fn backup(leaf_node: Node<Action, State>, reward: f64) {
    let mut node: Node<Action, State> = leaf_node;
    while let Some((parent, action)) = node.parent {
        node = parent;
        node.actions_Qs = (node.actions_counts(action) * node.actions_Qs(actions) + reward) / (node.actions_counts(action) + 1);
        node.actions_counts(action) += 1;
    }
}

fn best_move(&self) -> usize {
    *self.nodes[&self.root]
        .children
        .iter()
        .max_by(|&&a, &&b| {
            self.nodes[&a]
                .visits
                .partial_cmp(&self.nodes[&b].visits)
                .unwrap()
        })
        .unwrap()
}

fn mcts(&mut self, iterations: usize) {
    for _ in 0..iterations {
        let selected_node = self.select();
        let new_state = "new_state".to_string(); // Replace with your actual state generation logic
        let expanded_node = self.expand(selected_node, new_state);
        let result = self.simulate(expanded_node);
        self.backpropagate(expanded_node, result);
    }
}