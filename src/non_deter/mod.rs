use std::collections::{HashMap as Map, HashSet as Set};
use std::hash::Hash;
use std::iter::once;

use {Automaton, Deter};

#[derive(Debug, Clone)]
pub struct NonDeter<S, A>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    pub initial: Set<S>,
    pub accepting: Set<S>,
    pub transitions: Map<(S, Option<A>), Set<S>>,
}

impl<S, A> NonDeter<S, A>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    pub fn accepts_without_lambda<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = A>,
    {
        let states = self.traverse(input, self.initial.clone());

        for state in states.iter() {
            if self.accepting.get(state) != None {
                return true;
            }
        }

        false
    }

    pub fn traverse_without_lambda<I>(&self, input: I, mut states: Set<S>) -> Set<S>
    where
        I: Iterator<Item = A>,
    {
        for label in input {
            let mut new_states = Set::new();

            for from in states.into_iter() {
                if let Some(ref to) = self.transitions.get(&(from, Some(label.clone()))) {
                    new_states.extend(to.iter().cloned());
                }
            }

            states = new_states;
        }

        states
    }

    pub fn lambda_closure(&self, mut states: Set<S>) -> Set<S> {
        let mut not_checked: Vec<S> = states.iter().cloned().collect();

        while let Some(from) = not_checked.pop() {
            if let Some(ref to) = self.transitions.get(&(from, None)) {
                for state in to.iter() {
                    if states.get(state) == None {
                        states.insert(state.clone());
                        not_checked.push(state.clone());
                    }
                }
            }
        }

        states
    }

    pub fn eliminate_lambda(&self) -> NonDeter<S, A> {
        let mut no_lambda = NonDeter {
            initial: self.initial.clone(),
            accepting: self.accepting.clone(),
            transitions: Map::new(),
        };

        for ((from, label), _to) in self.transitions.iter() {
            if let Some(label) = label {
                let mut states = Set::new();
                states.insert(from.clone());

                states = self.traverse(once(label.clone()), states);

                no_lambda
                    .transitions
                    .insert((from.clone(), Some(label.clone())), states);
            }
        }

        no_lambda
    }

    pub fn make_deterministic(&self) -> Deter<usize, A> {
        let without_lambda = self.eliminate_lambda();

        // All the states and labels used by the automaton.
        let states = without_lambda.states();
        let labels = without_lambda.labels();

        let mut deter = Deter {
            initial: subset_as_usize(&states, &without_lambda.initial.clone()),
            accepting: Set::new(),
            transitions: Map::new(),
        };

        // The states of deter.
        let mut deter_states = Set::new();
        deter_states.insert(deter.initial);

        // We make a stack of sets of states that we have not covered.
        let mut not_checked = vec![without_lambda.initial.clone()];
        while let Some(from) = not_checked.pop() {
            let deter_from = subset_as_usize(&states, &from);

            // For each state and label we calculate the set of states that is reachable using the label.
            for label in labels.iter() {
                let to = without_lambda.traverse_without_lambda(once(label.clone()), from.clone());
                let deter_to = subset_as_usize(&states, &to);

                deter
                    .transitions
                    .insert((deter_from, label.clone()), deter_to);

                // If we have not covered the to state we push it the the stack and save it.
                if deter_states.get(&deter_to) == None {
                    deter_states.insert(deter_to);
                    not_checked.push(to);
                }
            }
        }

        // A set is accepting if its intersection with the accepting states is not empty.
        // We calculate the usize of the set of accepting states.
        let accepting_mask = subset_as_usize(&states, &without_lambda.accepting.clone());

        for state in deter_states.into_iter() {
            if state & accepting_mask != 0 {
                deter.accepting.insert(state);
            }
        }

        deter
    }
}

fn subset_as_usize<T>(set: &Set<T>, subset: &Set<T>) -> usize
where
    T: Eq + Hash,
{
    let mut result = 0;

    let mut power = 1;
    for element in set.iter() {
        if subset.get(element) != None {
            result += power;
        }
        power <<= 1;
    }

    result
}

impl<S, A> Automaton for NonDeter<S, A>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type State = S;
    type Alphabet = A;

    type Situation = Set<S>;

    fn accepts<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = A>,
    {
        let states = self.traverse(input, self.initial.clone());

        for state in states.iter() {
            if self.accepting.get(state) != None {
                return true;
            }
        }

        false
    }

    fn traverse<I>(&self, input: I, mut states: Set<S>) -> Set<S>
    where
        I: Iterator<Item = A>,
    {
        states = self.lambda_closure(states);

        for label in input {
            let mut new_states = Set::new();

            for from in states.into_iter() {
                if let Some(ref to) = self.transitions.get(&(from, Some(label.clone()))) {
                    new_states.extend(to.iter().cloned());
                }
            }

            states = self.lambda_closure(new_states);
        }

        states
    }

    fn states(&self) -> Set<S> {
        let mut states = Set::new();

        states.extend(self.initial.iter().cloned());
        states.extend(self.accepting.iter().cloned());

        for ((from, _label), to) in self.transitions.iter() {
            states.insert(from.clone());
            states.extend(to.iter().cloned());
        }

        states
    }

    fn reachable_states(&self) -> Set<S> {
        let labels = self.labels();

        let mut reachable_states = self.initial.clone();

        let mut not_checked: Vec<S> = self.initial.iter().cloned().collect();
        while let Some(from) = not_checked.pop() {
            for label in labels.iter() {
                let mut states = Set::new();
                states.insert(from.clone());

                states = self.traverse(once(label.clone()), states);

                for state in states {
                    if reachable_states.get(&state) == None {
                        reachable_states.insert(state.clone());
                        not_checked.push(state);
                    }
                }
            }
        }

        reachable_states
    }

    fn labels(&self) -> Set<A> {
        let mut labels = Set::new();

        for ((_from, label), _to) in self.transitions.iter() {
            if let Some(label) = label {
                labels.insert(label.clone());
            }
        }

        labels
    }
}
