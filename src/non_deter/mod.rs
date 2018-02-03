use std::collections::{HashMap as Map, HashSet as Set};
use std::hash::Hash;
use std::iter::once;
use std::ops;

use {Automaton, Deter};

#[derive(Debug, Clone)]
pub struct NonDeter<S, A>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    pub initial_states: Set<S>,
    pub accepting_states: Set<S>,
    pub transitions: Map<(S, Option<A>), Set<S>>,
}

impl<S, A> NonDeter<S, A>
where
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    pub fn traverse<I>(&self, input: I, mut states: Set<S>) -> Set<S>
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

    pub fn states(&self) -> Set<S> {
        let mut states = Set::new();

        states.extend(self.initial_states.iter().cloned());
        states.extend(self.accepting_states.iter().cloned());

        for ((from, _label), to) in self.transitions.iter() {
            states.insert(from.clone());
            states.extend(to.iter().cloned());
        }

        states
    }

    pub fn reachable_states(&self) -> Set<S> {
        let labels = self.labels();

        let mut reachable_states = self.initial_states.clone();

        let mut not_checked: Vec<S> = self.initial_states.iter().cloned().collect();
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

    pub fn labels(&self) -> Set<A> {
        let mut labels = Set::new();

        for ((_from, label), _to) in self.transitions.iter() {
            if let Some(label) = label {
                labels.insert(label.clone());
            }
        }

        labels
    }

    pub fn accepts_without_lambda<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = A>,
    {
        let states = self.traverse(input, self.initial_states.clone());

        for state in states.iter() {
            if self.accepting_states.get(state) != None {
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
            initial_states: self.initial_states.clone(),
            accepting_states: self.accepting_states.clone(),
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
            initial_state: subset_as_usize(&states, &without_lambda.initial_states.clone()),
            accepting_states: Set::new(),
            transitions: Map::new(),
        };

        // The states of deter.
        let mut deter_states = Set::new();
        deter_states.insert(deter.initial_state);

        // We make a stack of sets of states that we have not covered.
        let mut not_checked = vec![without_lambda.initial_states.clone()];
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

        // A set is accepting_states if its intersection with the accepting_states states is not empty.
        // We calculate the usize of the set of accepting_states states.
        let accepting_states_mask =
            subset_as_usize(&states, &without_lambda.accepting_states.clone());

        for state in deter_states.into_iter() {
            if state & accepting_states_mask != 0 {
                deter.accepting_states.insert(state);
            }
        }

        deter
    }

    fn combine<T>(&self, other: &NonDeter<T, A>) -> NonDeter<(S, T), A>
    where
        T: Clone + Eq + Hash,
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
        T: Clone + Eq + Hash,
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
        T: Clone + Eq + Hash,
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
        T: Clone + Eq + Hash,
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
        T: Clone + Eq + Hash,
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

fn product<T, U>(left_set: &Set<T>, right_set: &Set<U>) -> Set<(T, U)>
where
    T: Clone + Eq + Hash,
    U: Clone + Eq + Hash,
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
    S: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type Alphabet = A;

    fn accepts<I>(&self, input: I) -> bool
    where
        I: Iterator<Item = A>,
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

impl<'a, 'b, S, T, A> ops::BitOr<&'b NonDeter<T, A>> for &'a NonDeter<S, A>
where
    S: Clone + Eq + Hash,
    T: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type Output = NonDeter<(S, T), A>;

    fn bitor(self, rhs: &NonDeter<T, A>) -> NonDeter<(S, T), A> {
        self.union(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::BitAnd<&'b NonDeter<T, A>> for &'a NonDeter<S, A>
where
    S: Clone + Eq + Hash,
    T: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type Output = NonDeter<(S, T), A>;

    fn bitand(self, rhs: &NonDeter<T, A>) -> NonDeter<(S, T), A> {
        self.intersection(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::BitXor<&'b NonDeter<T, A>> for &'a NonDeter<S, A>
where
    S: Clone + Eq + Hash,
    T: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type Output = NonDeter<(S, T), A>;

    fn bitxor(self, rhs: &NonDeter<T, A>) -> NonDeter<(S, T), A> {
        self.symmetric_difference(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::Sub<&'b NonDeter<T, A>> for &'a NonDeter<S, A>
where
    S: Clone + Eq + Hash,
    T: Clone + Eq + Hash,
    A: Clone + Eq + Hash,
{
    type Output = NonDeter<(S, T), A>;

    fn sub(self, rhs: &NonDeter<T, A>) -> NonDeter<(S, T), A> {
        self.difference(rhs)
    }
}
