use std::collections::{HashMap as Map, HashSet as Set};
use std::hash::Hash;

use Automaton;

#[derive(Debug, Clone)]
pub struct PushDown<S, A, T>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
{
    pub initial_states: Set<S>,
    pub initial_stack: Vec<T>,
    pub accepting_states: Set<S>,
    pub transitions: Map<(S, Option<A>, Option<T>), Set<(S, Vec<T>)>>,
}

impl<S, A, T> PushDown<S, A, T>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
{
    pub fn traverse<I>(&self, input: I, mut states: Set<(S, Vec<T>)>) -> Set<(S, Vec<T>)>
    where
        I: IntoIterator<Item = A>,
    {
        states = self.lambda_closure(states);

        for label in input.into_iter() {
            let mut new_states = Set::new();

            for (from, stack) in states.into_iter() {
                let mut stack = stack.clone();
                if let Some(to) = self.transitions
                    .get(&(from, Some(label.clone()), stack.pop()))
                {
                    new_states.extend(to.iter().map(|(new_state, new_stack)| {
                        stack.extend_from_slice(new_stack);
                        (new_state.clone(), stack.clone())
                    }));
                }
            }

            states = self.lambda_closure(new_states);
        }

        states
    }

    pub fn lambda_closure(&self, mut states: Set<(S, Vec<T>)>) -> Set<(S, Vec<T>)> {
        let mut not_checked: Vec<(S, Vec<T>)> = states.iter().cloned().collect();

        while let Some((from, stack)) = not_checked.pop() {
            if let Some(ref to) = self.transitions
                .get(&(from.clone(), None, stack.last().cloned()))
            {
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

    pub fn states(&self) -> Set<S> {
        let mut states = Set::new();

        states.extend(self.initial_states.iter().cloned());

        for to in self.transitions.values() {
            states.extend(to.iter().map(|(state, _stack)| state.clone()));
        }

        states
    }

    pub fn used_states(&self) -> Set<S> {
        let labels = self.labels();
        let symbols = self.symbols();

        let mut used_states = self.initial_states.clone();

        let mut not_checked: Vec<S> = self.initial_states.iter().cloned().collect();
        while let Some(from) = not_checked.pop() {
            for label in labels.iter() {
                for top in symbols.iter() {
                    let mut states = Set::new();
                    states.insert((from.clone(), vec![top.clone()]));

                    states = self.traverse(Some(label.clone()), states);

                    for (state, _top) in states {
                        if used_states.get(&state) == None {
                            used_states.insert(state.clone());
                            not_checked.push(state);
                        }
                    }
                }

                let mut states = Set::new();
                states.insert((from.clone(), vec![]));

                states = self.traverse(Some(label.clone()), states);

                for (state, _top) in states {
                    if used_states.get(&state) == None {
                        used_states.insert(state.clone());
                        not_checked.push(state);
                    }
                }
            }
        }

        used_states
    }

    pub fn labels(&self) -> Set<A> {
        let mut labels = Set::new();

        for (_from, label, _top) in self.transitions.keys() {
            if let Some(label) = label {
                labels.insert(label.clone());
            }
        }

        labels
    }

    pub fn used_labels(&self) -> Set<A> {
        let used_states = self.used_states();

        let mut used_labels = Set::new();

        for (from, label, _top) in self.transitions.keys() {
            if let Some(label) = label {
                if used_states.get(from) != None {
                    used_labels.insert(label.clone());
                }
            }
        }

        used_labels
    }

    pub fn symbols(&self) -> Set<T> {
        let mut symbols = Set::new();

        for (_from, _label, top) in self.transitions.keys() {
            if let Some(top) = top {
                symbols.insert(top.clone());
            }
        }

        symbols
    }

    pub fn used_symbols(&self) -> Set<T> {
        let used_states = self.used_states();

        let mut used_symbols = Set::new();

        for (from, _label, top) in self.transitions.keys() {
            if let Some(top) = top {
                if used_states.get(from) != None {
                    used_symbols.insert(top.clone());
                }
            }
        }

        used_symbols
    }

    pub fn accepts_empty_stack<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = A>,
    {
        let states = self.traverse(
            input,
            self.initial_states
                .iter()
                .map(|state| (state.clone(), self.initial_stack.clone()))
                .collect(),
        );

        for (_state, stack) in states.iter() {
            if stack.is_empty() {
                return true;
            }
        }

        false
    }
}

impl<S, A, T> Automaton for PushDown<S, A, T>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Alphabet = A;

    fn accepts<I>(&self, input: I) -> bool
    where
        I: IntoIterator<Item = A>,
    {
        let states = self.traverse(
            input,
            self.initial_states
                .iter()
                .map(|state| (state.clone(), self.initial_stack.clone()))
                .collect(),
        );

        for (state, _stack) in states.iter() {
            if self.accepting_states.get(state) != None {
                return true;
            }
        }

        false
    }
}
