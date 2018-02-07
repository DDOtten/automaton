use std::collections::{HashMap as Map, HashSet as Set};
use std::hash::Hash;
use std::ops;

use Automaton;

#[derive(Debug, Clone)]
pub struct NonDeter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    pub initial_states: Set<S>,
    pub accepting_states: Set<S>,
    pub transitions: Map<(S, Option<A>), Set<S>>,
}

impl<S, A> NonDeter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    pub fn traverse<I>(&self, input: I, mut states: Set<S>) -> Set<S>
    where
        I: IntoIterator<Item = A>,
    {
        states = self.epsilon_closure(states);

        for label in input.into_iter() {
            let mut new_states = Set::new();

            for from in states.into_iter() {
                if let Some(ref to) = self.transitions.get(&(from, Some(label.clone()))) {
                    new_states.extend(to.iter().cloned());
                }
            }

            states = self.epsilon_closure(new_states);
        }

        states
    }

    pub fn epsilon_closure(&self, mut states: Set<S>) -> Set<S> {
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

    pub fn states(&self) -> Set<S> {
        let mut states = Set::new();

        states.extend(self.initial_states.iter().cloned());

        for to in self.transitions.values() {
            states.extend(to.iter().cloned());
        }

        states
    }

    pub fn used_states(&self) -> Set<S> {
        let labels = self.labels();

        let mut used_states = self.initial_states.clone();

        let mut not_checked: Vec<S> = self.initial_states.iter().cloned().collect();
        while let Some(from) = not_checked.pop() {
            for label in labels.iter() {
                let mut states = Set::new();
                states.insert(from.clone());

                states = self.traverse(Some(label.clone()), states);

                for state in states {
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

        for (_from, label) in self.transitions.keys() {
            if let Some(label) = label {
                labels.insert(label.clone());
            }
        }

        labels
    }

    pub fn used_labels(&self) -> Set<A> {
        let used_states = self.used_states();

        let mut used_labels = Set::new();

        for (from, label) in self.transitions.keys() {
            if let Some(label) = label {
                if used_states.get(from) != None {
                    used_labels.insert(label.clone());
                }
            }
        }

        used_labels
    }

    pub fn no_epsilon(&self) -> NonDeter<S, A> {
        let mut no_epsilon = NonDeter {
            initial_states: self.initial_states.clone(),
            accepting_states: self.accepting_states.clone(),
            transitions: Map::new(),
        };

        for (from, label) in self.transitions.keys() {
            if let Some(label) = label {
                let mut states = Set::new();
                states.insert(from.clone());

                states = self.traverse(Some(label.clone()), states);

                no_epsilon
                    .transitions
                    .insert((from.clone(), Some(label.clone())), states);
            }
        }

        no_epsilon
    }

    pub fn accepts_no_epsilon<I>(&self, input: I) -> bool
    where
        I: IntoIterator<Item = A>,
    {
        let states = self.traverse(input, self.initial_states.clone());

        for state in states.iter() {
            if self.accepting_states.get(state) != None {
                return true;
            }
        }

        false
    }

    pub fn traverse_no_epsilon<I>(&self, input: I, mut states: Set<S>) -> Set<S>
    where
        I: IntoIterator<Item = A>,
    {
        for label in input.into_iter() {
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

    pub fn kleene_star(mut self) -> NonDeter<S, A> {
        for accepting in self.accepting_states.iter().cloned() {
            match self.transitions.remove(&(accepting.clone(), None)) {
                Some(mut states) => {
                    states.extend(self.initial_states.iter().cloned());
                    self.transitions.insert((accepting, None), states);
                },
                None => {
                    self.transitions
                        .insert((accepting, None), self.initial_states.clone());
                },
            }
        }

        self
    }

    fn combine<T>(&self, other: &NonDeter<T, A>) -> NonDeter<(S, T), A>
    where
        T: Clone + Eq + Hash + ::std::fmt::Debug,
    {
        let initial_states = product(&self.initial_states, &other.initial_states);

        let mut transitions = Map::new();
        for ((self_from, self_label), self_to) in self.transitions.iter() {
            for ((other_from, other_label), other_to) in other.transitions.iter() {
                if self_label == other_label {
                    transitions.insert(
                        ((self_from.clone(), other_from.clone()), self_label.clone()),
                        product(self_to, other_to),
                    );
                }
            }
        }

        NonDeter {
            initial_states,
            accepting_states: Set::new(),
            transitions,
        }
    }

    pub fn union<'a, T>(&'a self, other: &'a NonDeter<T, A>) -> NonDeter<(S, T), A>
    where
        T: Clone + Eq + Hash + ::std::fmt::Debug,
    {
        let mut automaton = NonDeter::combine(&self, &other);

        for self_state in self.accepting_states.iter() {
            for other_state in other.states() {
                automaton
                    .accepting_states
                    .insert((self_state.clone(), other_state.clone()));
            }
        }
        for other_state in other.accepting_states.iter() {
            for self_state in self.states() {
                automaton
                    .accepting_states
                    .insert((self_state.clone(), other_state.clone()));
            }
        }

        automaton
    }

    pub fn intersection<'a, T>(&'a self, other: &'a NonDeter<T, A>) -> NonDeter<(S, T), A>
    where
        T: Clone + Eq + Hash + ::std::fmt::Debug,
    {
        let mut automaton = NonDeter::combine(&self, &other);

        for self_state in self.accepting_states.iter() {
            for other_state in other.accepting_states.iter() {
                automaton
                    .accepting_states
                    .insert((self_state.clone(), other_state.clone()));
            }
        }

        automaton
    }

    pub fn difference<'a, T>(&'a self, other: &'a NonDeter<T, A>) -> NonDeter<(S, T), A>
    where
        T: Clone + Eq + Hash + ::std::fmt::Debug,
    {
        let mut automaton = NonDeter::combine(&self, &other);

        for self_state in self.accepting_states.iter() {
            for other_state in other.states() {
                if other.accepting_states.get(&other_state) == None {
                    automaton
                        .accepting_states
                        .insert((self_state.clone(), other_state.clone()));
                }
            }
        }

        automaton
    }

    pub fn symmetric_difference<'a, T>(&'a self, other: &'a NonDeter<T, A>) -> NonDeter<(S, T), A>
    where
        T: Clone + Eq + Hash + ::std::fmt::Debug,
    {
        let mut automaton = NonDeter::combine(&self, &other);

        let mut union = Set::new();
        for self_state in self.accepting_states.iter() {
            for other_state in other.states() {
                union.insert((self_state.clone(), other_state.clone()));
            }
        }
        for other_state in other.accepting_states.iter() {
            for self_state in self.states() {
                union.insert((self_state.clone(), other_state.clone()));
            }
        }

        let mut intersection = Set::new();
        for self_state in self.accepting_states.iter() {
            for other_state in other.accepting_states.iter() {
                intersection.insert((self_state.clone(), other_state.clone()));
            }
        }

        automaton.accepting_states = &union - &intersection;

        automaton
    }
}

fn product<T, U>(left_set: &Set<T>, right_set: &Set<U>) -> Set<(T, U)>
where
    T: Clone + Eq + Hash + ::std::fmt::Debug,
    U: Clone + Eq + Hash + ::std::fmt::Debug,
{
    let mut result = Set::new();

    for left in left_set.iter() {
        for right in right_set.iter() {
            result.insert((left.clone(), right.clone()));
        }
    }

    result
}

impl<S, A> Automaton for NonDeter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Alphabet = A;

    fn accepts<I>(&self, input: I) -> bool
    where
        I: IntoIterator<Item = A>,
    {
        let states = self.traverse(input, self.initial_states.clone());

        for state in states.iter() {
            if self.accepting_states.get(state) != None {
                return true;
            }
        }

        false
    }
}

impl<S, A> From<::Deter<S, A>> for NonDeter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    fn from(deter: ::Deter<S, A>) -> NonDeter<S, A> {
        let ::Deter {
            initial_state,
            accepting_states,
            transitions,
        } = deter;

        let mut initial_states = Set::new();
        initial_states.insert(initial_state);

        let transitions = transitions
            .into_iter()
            .map(|((from, label), to)| {
                let mut states = Set::new();
                states.insert(to);

                ((from, Some(label)), states)
            })
            .collect();

        NonDeter {
            initial_states,
            accepting_states,
            transitions,
        }
    }
}

impl<'a, 'b, S, T, A> ops::BitOr<&'b NonDeter<T, A>> for &'a NonDeter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Output = NonDeter<(S, T), A>;

    fn bitor(self, rhs: &NonDeter<T, A>) -> NonDeter<(S, T), A> {
        self.union(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::BitAnd<&'b NonDeter<T, A>> for &'a NonDeter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Output = NonDeter<(S, T), A>;

    fn bitand(self, rhs: &NonDeter<T, A>) -> NonDeter<(S, T), A> {
        self.intersection(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::BitXor<&'b NonDeter<T, A>> for &'a NonDeter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Output = NonDeter<(S, T), A>;

    fn bitxor(self, rhs: &NonDeter<T, A>) -> NonDeter<(S, T), A> {
        self.symmetric_difference(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::Sub<&'b NonDeter<T, A>> for &'a NonDeter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Output = NonDeter<(S, T), A>;

    fn sub(self, rhs: &NonDeter<T, A>) -> NonDeter<(S, T), A> {
        self.difference(rhs)
    }
}
