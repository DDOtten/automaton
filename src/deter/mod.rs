use std::collections::{HashMap as Map, HashSet as Set};
use std::hash::Hash;
use std::ops;

use Automaton;

#[derive(Debug, Clone)]
pub struct Deter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    /// The initial state of the automaton.
    pub initial_state: S,
    /// The set of accepting states.
    pub accepting_states: Set<S>,
    /// A transition goes from a state and a label to another state.
    pub transitions: Map<(S, A), S>,
}

impl<S, A> Deter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    /// Traverses the automata using the given input starting at the given state.
    ///
    /// Returnes the state the automaton ends up in.
    ///
    /// # Examples
    ///
    /// ```
    /// //       a                   a
    /// //     ╭───╮               ╭───╮
    /// //     │   ▽  a            │   ▽
    /// //     ╭───╮ ──▷ ╭───╮  a  ╔═══╗ ──╮
    /// // ──▷ │ 0 │     │ 1 │ ──▷ ║ 2 ║   │ a,b
    /// //     ╰───╯ ◁── ╰───╯     ╚═══╝ ◁─╯
    /// //       △    b              │
    /// //       ╰───────────────────╯
    /// //                 b
    ///
    /// extern crate automaton;
    /// use automaton::Automaton;
    ///
    /// # fn main() {
    /// let deter = automaton::Deter {
    ///     initial_state: 0,
    ///     accepting_states: vec![
    ///         2,
    ///     ].into_iter().collect(),
    ///     transitions: vec![
    ///         ((0, 'a'), 1),
    ///         ((0, 'b'), 0),
    ///         ((1, 'a'), 2),
    ///         ((1, 'b'), 0),
    ///         ((2, 'a'), 2),
    ///         ((2, 'b'), 0),
    ///     ].into_iter().collect(),
    /// };
    ///
    /// assert!(deter.traverse("bab".chars(), 0) == Some(0));
    /// assert!(deter.traverse("aba".chars(), 1) == Some(1));
    /// assert!(deter.traverse("bab".chars(), 2) == Some(0));
    /// # }
    /// ```
    pub fn traverse<I>(&self, input: I, mut state: S) -> Option<S>
    where
        I: IntoIterator<Item = A>,
    {
        for label in input.into_iter() {
            state = self.transitions.get(&(state, label))?.clone();
        }

        Some(state)
    }

    /// A quick calculation of states the automaton could end up in.
    ///
    /// Returnes the initial state and all states that are the result of a transition.
    ///
    /// # Examples
    ///
    /// ```
    /// //     ╭───╮     ╔═══╗ ──╮
    /// // ──▷ │ ! │     ║ @ ║   │ a,b
    /// //     ╰───╯     ╚═══╝ ◁─╯
    /// //       │
    /// //       │ a
    /// //       ▽
    /// //     ╭───╮  a  ╔═══╗     ╔═══╗
    /// //     │ # │ ──▷ ║ $ ║     ║ % ║
    /// //     ╰───╯     ╚═══╝     ╚═══╝
    ///
    /// extern crate automaton;
    /// use automaton::Automaton;
    ///
    /// # fn main() {
    /// let deter = automaton::Deter {
    ///     initial_state: '!',
    ///     accepting_states: vec![
    ///         '@',
    ///         '$',
    ///         '%',
    ///     ].into_iter().collect(),
    ///     transitions: vec![
    ///         (('!', 'a'), '#'),
    ///         (('#', 'a'), '$'),
    ///         (('@', 'a'), '@'),
    ///         (('@', 'b'), '@'),
    ///     ].into_iter().collect(),
    /// };
    ///
    /// assert!(deter.states() == vec!['!', '@', '#', '$'].into_iter().collect());
    /// # }
    /// ```
    pub fn states(&self) -> Set<S> {
        let mut states = Set::new();

        states.insert(self.initial_state.clone());

        for to in self.transitions.values() {
            states.insert(to.clone());
        }

        states
    }

    /// All states that are reachable from the initial state.
    ///
    /// # Examples
    ///
    /// ```
    /// //     ╭───╮     ╔═══╗ ──╮
    /// // ──▷ │ ! │     ║ @ ║   │ a,b
    /// //     ╰───╯     ╚═══╝ ◁─╯
    /// //       │
    /// //       │ a
    /// //       ▽
    /// //     ╭───╮  a  ╔═══╗     ╔═══╗
    /// //     │ # │ ──▷ ║ $ ║     ║ % ║
    /// //     ╰───╯     ╚═══╝     ╚═══╝
    ///
    /// extern crate automaton;
    /// use automaton::Automaton;
    ///
    /// # fn main() {
    /// let deter = automaton::Deter {
    ///     initial_state: '!',
    ///     accepting_states: vec![
    ///         '@',
    ///         '$',
    ///         '%',
    ///     ].into_iter().collect(),
    ///     transitions: vec![
    ///         (('!', 'a'), '#'),
    ///         (('#', 'a'), '$'),
    ///         (('@', 'a'), '@'),
    ///         (('@', 'b'), '@'),
    ///     ].into_iter().collect(),
    /// };
    ///
    /// assert!(deter.used_states() == vec!['!', '#', '$'].into_iter().collect());
    /// # }
    /// ```
    pub fn used_states(&self) -> Set<S> {
        let labels = self.labels();

        let mut used_states = Set::new();
        used_states.insert(self.initial_state.clone());

        let mut not_checked = vec![&self.initial_state];
        while let Some(from) = not_checked.pop() {
            for label in labels.iter() {
                if let Some(to) = self.transitions.get(&(from.clone(), label.clone())) {
                    if used_states.get(to) == None {
                        used_states.insert(to.clone());
                        not_checked.push(to);
                    }
                }
            }
        }

        used_states
    }

    /// A quick calculation of the labels the automaton uses.
    ///
    /// Returnes every label that is used in a transition.
    ///
    /// # Examples
    ///
    /// ```
    /// //     ╭───╮     ╔═══╗ ──╮
    /// // ──▷ │ ! │     ║ @ ║   │ a,b
    /// //     ╰───╯     ╚═══╝ ◁─╯
    /// //       │
    /// //       │ a
    /// //       ▽
    /// //     ╭───╮  a  ╔═══╗     ╔═══╗
    /// //     │ # │ ──▷ ║ $ ║     ║ % ║
    /// //     ╰───╯     ╚═══╝     ╚═══╝
    ///
    /// extern crate automaton;
    /// use automaton::Automaton;
    ///
    /// # fn main() {
    /// let deter = automaton::Deter {
    ///     initial_state: '!',
    ///     accepting_states: vec![
    ///         '@',
    ///         '$',
    ///         '%',
    ///     ].into_iter().collect(),
    ///     transitions: vec![
    ///         (('!', 'a'), '#'),
    ///         (('#', 'a'), '$'),
    ///         (('@', 'a'), '@'),
    ///         (('@', 'b'), '@'),
    ///     ].into_iter().collect(),
    /// };
    ///
    /// assert!(deter.labels() == vec!['a', 'b'].into_iter().collect());
    /// # }
    /// ```
    pub fn labels(&self) -> Set<A> {
        self.transitions
            .keys()
            .map(|(_from, label)| label.clone())
            .collect()
    }

    /// All labels that are used in a transition between reachable states.
    ///
    /// # Examples
    ///
    /// ```
    /// //     ╭───╮     ╔═══╗ ──╮
    /// // ──▷ │ ! │     ║ @ ║   │ a,b
    /// //     ╰───╯     ╚═══╝ ◁─╯
    /// //       │
    /// //       │ a
    /// //       ▽
    /// //     ╭───╮  a  ╔═══╗     ╔═══╗
    /// //     │ # │ ──▷ ║ $ ║     ║ % ║
    /// //     ╰───╯     ╚═══╝     ╚═══╝
    ///
    /// extern crate automaton;
    /// use automaton::Automaton;
    ///
    /// # fn main() {
    /// let deter = automaton::Deter {
    ///     initial_state: '!',
    ///     accepting_states: vec![
    ///         '@',
    ///         '$',
    ///         '%',
    ///     ].into_iter().collect(),
    ///     transitions: vec![
    ///         (('!', 'a'), '#'),
    ///         (('#', 'a'), '$'),
    ///         (('@', 'a'), '@'),
    ///         (('@', 'b'), '@'),
    ///     ].into_iter().collect(),
    /// };
    ///
    /// assert!(deter.used_labels() == vec!['a'].into_iter().collect());
    /// # }
    /// ```
    pub fn used_labels(&self) -> Set<A> {
        let used_states = self.used_states();

        self.transitions
            .keys()
            .filter_map(|(from, label)| {
                if used_states.get(from) == None {
                    None
                } else {
                    Some(label.clone())
                }
            })
            .collect()
    }

    /// Returnes the automaton with the minimal amount of states that accepts the same language.
    ///
    /// # Examples
    ///
    /// The minimal automaton that accepts precisely one `1`.
    ///
    /// ```
    /// // Initial:        0
    /// //               ╭───╮
    /// //               │   ▽
    /// //     ╭───╮  1  ╔═══╗         1     ╔═══╗
    /// // ──▷ │ a │ ──▷ ║ c ║ ──────╮       ║ f ║
    /// //     ╰───╯     ╚═══╝       │       ╚═══╝
    /// //     △   │       △         │       △   │
    /// //   0 │   │ 0   0 │         │       ╰───╯
    /// //     │   ▽       │         ▽        0,1
    /// //     ╭───╮  1  ╔═══╗  1  ╭───╮ ──╮
    /// //     │ b │ ──▷ ║ d ║ ──▷ │ e │   │ 0,1
    /// //     ╰───╯     ╚═══╝     ╰───╯ ◁─╯
    /// //
    /// // Minimal:
    /// //     ╭───╮  1  ╔═══╗  1  ╭───╮
    /// // ──▷ │   │ ──▷ ║   ║ ──▷ │   │
    /// //     ╰───╯     ╚═══╝     ╰───╯
    /// //     △   │     △   │     △   │
    /// //     ╰───╯     ╰───╯     ╰───╯
    /// //       0         0        1,0
    ///
    /// extern crate automaton;
    /// use automaton::Automaton;
    ///
    /// # fn main() {
    /// let deter = automaton::Deter {
    ///     initial_state: 'a',
    ///     accepting_states: vec![
    ///         'c',
    ///         'd',
    ///         'f',
    ///     ].into_iter().collect(),
    ///     transitions: vec![
    ///         (('a', 0), 'b'),
    ///         (('a', 1), 'c'),
    ///         (('b', 0), 'a'),
    ///         (('b', 1), 'd'),
    ///         (('c', 0), 'c'),
    ///         (('c', 1), 'e'),
    ///         (('d', 0), 'c'),
    ///         (('d', 1), 'e'),
    ///         (('e', 0), 'e'),
    ///         (('e', 1), 'e'),
    ///         (('f', 0), 'f'),
    ///         (('f', 1), 'f'),
    ///     ].into_iter().collect(),
    /// };
    ///
    /// let minimal = deter.minimal();
    ///
    /// assert!(minimal.states().len() == 3);
    /// assert!(minimal.accepting_states.len() == 1);
    ///
    /// assert!(minimal.accepts(vec![0, 0, 0, 1, 0, 0]));
    /// assert!(minimal.accepts(vec![1, 0]));
    /// assert!(!minimal.accepts(vec![1, 0, 1]));
    /// assert!(!minimal.accepts(vec![]));
    /// # }
    /// ```
    pub fn minimal(&self) -> Deter<usize, A> {
        let labels = self.labels();
        let used_states = self.used_states();

        let mut similar = Map::new();

        // We create all the unique pairs of states and mark them similar if and only if
        // they are both accepting_states or both not accepting_states.
        let mut iter1 = used_states.iter();
        while let Some(state1) = iter1.next() {
            let mut iter2 = iter1.clone();
            while let Some(state2) = iter2.next() {
                similar.insert(
                    (state1, state2),
                    (self.accepting_states.get(state1) == None)
                        == (self.accepting_states.get(state2) == None),
                );
            }
        }

        // We mark states not similar if there is a label where the resuling states are
        // not similar. We keep repeating this while we have made a change in
        // the last iteration.
        let mut changed = true;
        while changed {
            changed = false;

            let mut iter1 = used_states.iter();
            while let Some(from1) = iter1.next() {
                let mut iter2 = iter1.clone();
                while let Some(from2) = iter2.next() {
                    // If the states are marked similar.
                    if similar.get(&(from1, from2)) == Some(&true) {
                        for label in labels.iter() {
                            if let (Some(to1), Some(to2)) = (
                                self.transitions.get(&(from1.clone(), label.clone())),
                                self.transitions.get(&(from2.clone(), label.clone())),
                            ) {
                                // If we can reach states that are not marked similar.
                                if similar.get(&(to1, to2)) == Some(&false)
                                    || similar.get(&(to2, to1)) == Some(&false)
                                {
                                    similar.insert((from1, from2), false);
                                    changed = true;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut partition: Map<&S, usize> = Map::new();

        // For each state, if we have already maped a state similar to it we map it to
        // the same value, else we map it to a new value.
        let mut max = 0;
        for state2 in used_states.iter() {
            partition.insert(state2, {
                let mut ret = max;

                for state1 in used_states.iter() {
                    // If we have not found a similar state that is already maped.
                    if state1 == state2 {
                        max += 1;
                        break;
                    // If we do find such a state.
                    } else if similar.get(&(state1, state2)) == Some(&true) {
                        // Because state1 is earlier in the iterator of used states we can be sure
                        // it already has a maped value.
                        let value = partition.get(state1).unwrap().clone();
                        ret = value;
                        break;
                    }
                }

                ret
            });
        }

        Deter {
            initial_state: *partition.get(&self.initial_state).unwrap(),
            accepting_states: self.accepting_states
                .iter()
                .filter_map(|state| partition.get(state).cloned())
                .collect(),
            transitions: self.transitions
                .iter()
                .filter_map(|((from, label), to)| {
                    Some(((*partition.get(from)?, label.clone()), *partition.get(to)?))
                })
                .collect(),
        }
    }

    /// Returnes a automaton accepting the same language with a transition for each state and label.
    ///
    /// For every missing transition inserts a transition to a new `None` state.
    ///
    /// # Examples
    ///
    /// The complete automaton that accepts only `ab`.
    ///
    /// ```
    /// // Initial:
    /// //     ╭───╮  a  ╭───╮  b  ╔═══╗
    /// // ──▷ │ 0 │ ──▷ │ 1 │ ──▷ ║ 2 ║
    /// //     ╰───╯     ╰───╯     ╚═══╝
    /// //
    /// // Complete:
    /// //     ╭───╮  a  ╭───╮  b  ╔═══╗
    /// // ──▷ │ 0 │ ──▷ │ 1 │ ──▷ ║ 2 ║
    /// //     ╰───╯     ╰───╯     ╚═══╝
    /// //       │         │         │
    /// //       │         │ a       │
    /// //       │         ▽         │
    /// //       │ b     ╭───╮       │
    /// //       ╰─────▷ │ N │ ◁─────╯
    /// //               ╰───╯        a,b
    /// //               △   │
    /// //               ╰───╯
    /// //                a,b
    ///
    /// extern crate automaton;
    /// use automaton::Automaton;
    ///
    /// # fn main() {
    /// let deter = automaton::Deter {
    ///     initial_state: 0,
    ///     accepting_states: vec![
    ///         2,
    ///     ].into_iter().collect(),
    ///     transitions: vec![
    ///         ((0, 'a'), 1),
    ///         ((1, 'b'), 2),
    ///     ].into_iter().collect(),
    /// };
    ///
    /// let complete = deter.complete();
    ///
    /// assert!(complete.states().len() == 4);
    /// assert!(complete.transitions.len() == 8);
    ///
    /// assert!(complete.accepts("ab".chars()));
    /// assert!(!complete.accepts("aba".chars()));
    /// # }
    /// ```
    pub fn complete(self) -> Deter<Option<S>, A> {
        let states = self.states();
        let labels = self.labels();

        let Deter {
            initial_state,
            accepting_states,
            transitions,
        } = self;

        Deter {
            initial_state: Some(initial_state),
            accepting_states: accepting_states
                .into_iter()
                .map(|state| Some(state))
                .collect(),
            transitions: {
                let mut missing_transitions = Map::new();

                for state in states.iter() {
                    for label in labels.iter() {
                        if transitions.get(&(state.clone(), label.clone())) == None {
                            missing_transitions.insert((Some(state.clone()), label.clone()), None);
                        }
                    }
                }

                missing_transitions.extend(
                    transitions
                        .into_iter()
                        .map(|((from, label), to)| ((Some(from), label), Some(to))),
                );

                missing_transitions.extend(labels.into_iter().map(|label| ((None, label), None)));

                missing_transitions
            },
        }
    }

    /// Returnes a automaton that accepts precisely the things that `self` does not accept.
    ///
    /// All transitions in self must be present for this to work.
    ///
    /// # Examples
    ///
    /// The automaton that accepts an uneven number of `1`'s is converted to the automaton that
    /// accepts an even number of `1`'s.
    ///
    /// ```
    /// // Initial:
    /// //            1
    /// //     ╭───╮ ──▷ ╔═══╗
    /// // ──▷ │ 0 │     ║ 1 ║
    /// //     ╰───╯ ◁── ╚═══╝
    /// //     △   │  1  △   │
    /// //     ╰───╯     ╰───╯
    /// //       0         0
    /// //
    /// // Complement:
    /// //            1
    /// //     ╔═══╗ ──▷ ╭───╮
    /// // ──▷ ║ 0 ║     │ 1 │
    /// //     ╚═══╝ ◁── ╰───╯
    /// //     △   │  1  △   │
    /// //     ╰───╯     ╰───╯
    /// //       0         0
    ///
    /// extern crate automaton;
    /// use automaton::Automaton;
    ///
    /// # fn main() {
    /// let deter = automaton::Deter {
    ///     initial_state: 0,
    ///     accepting_states: vec![
    ///         1,
    ///     ].into_iter().collect(),
    ///     transitions: vec![
    ///         ((0, 0), 0),
    ///         ((0, 1), 1),
    ///         ((1, 0), 1),
    ///         ((1, 1), 0),
    ///     ].into_iter().collect(),
    /// };
    ///
    /// let complement = deter.complement();
    ///
    /// assert!(complement.accepts(vec![0, 1, 0, 1, 0, 0]));
    /// assert!(complement.accepts(vec![]));
    /// assert!(!complement.accepts(vec![1, 0, 0]));
    /// assert!(!complement.accepts(vec![1]));
    /// # }
    /// ```
    pub fn complement(mut self) -> Deter<S, A> {
        self.accepting_states = &self.states() - &self.accepting_states;

        self
    }
    fn combine<T>(&self, other: &Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash + ::std::fmt::Debug,
    {
        let initial_state = (self.initial_state.clone(), other.initial_state.clone());

        let mut transitions = Map::new();
        for ((self_from, self_label), self_to) in self.transitions.iter() {
            for ((other_from, other_label), other_to) in other.transitions.iter() {
                if self_label == other_label {
                    transitions.insert(
                        ((self_from.clone(), other_from.clone()), self_label.clone()),
                        (self_to.clone(), other_to.clone()),
                    );
                }
            }
        }

        Deter {
            initial_state,
            accepting_states: Set::new(),
            transitions,
        }
    }

    pub fn union<'a, T>(&'a self, other: &'a Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash + ::std::fmt::Debug,
    {
        let mut automaton = Deter::combine(&self, &other);

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

    pub fn intersection<'a, T>(&'a self, other: &'a Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash + ::std::fmt::Debug,
    {
        let mut automaton = Deter::combine(&self, &other);

        for self_state in self.accepting_states.iter() {
            for other_state in other.accepting_states.iter() {
                automaton
                    .accepting_states
                    .insert((self_state.clone(), other_state.clone()));
            }
        }

        automaton
    }

    pub fn difference<'a, T>(&'a self, other: &'a Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash + ::std::fmt::Debug,
    {
        let mut automaton = Deter::combine(&self, &other);

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

    pub fn symmetric_difference<'a, T>(&'a self, other: &'a Deter<T, A>) -> Deter<(S, T), A>
    where
        T: Clone + Eq + Hash + ::std::fmt::Debug,
    {
        let mut automaton = Deter::combine(&self, &other);

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

impl<S, A> Automaton for Deter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Alphabet = A;

    fn accepts<I>(&self, input: I) -> bool
    where
        I: IntoIterator<Item = A>,
    {
        match self.traverse(input, self.initial_state.clone()) {
            Some(state) => self.accepting_states.get(&state) != None,
            None => false,
        }
    }
}

impl<S, A> From<::NonDeter<S, A>> for Deter<usize, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    fn from(non_deter: ::NonDeter<S, A>) -> Deter<usize, A> {
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

        let states = non_deter.states();
        let labels = non_deter.labels();

        let ::NonDeter {
            initial_states,
            accepting_states,
            transitions,
        } = non_deter.no_epsilon();

        let mut deter = Deter {
            initial_state: subset_as_usize(&states, &initial_states),
            accepting_states: Set::new(),
            transitions: Map::new(),
        };

        let mut deter_states = Set::new();
        deter_states.insert(deter.initial_state);

        // We make a stack of sets of states that we have not covered.
        let mut not_checked = vec![initial_states];
        while let Some(from) = not_checked.pop() {
            let deter_from = subset_as_usize(&states, &from);

            // For each state and label we calculate the set of states that is reachable
            // using the label.
            for label in labels.iter() {
                let mut to = Set::new();

                for from in from.iter().cloned() {
                    if let Some(ref states) = transitions.get(&(from, Some(label.clone()))) {
                        to.extend(states.iter().cloned());
                    }
                }

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

        // A set is accepting if its intersection with the accepting states is
        // not empty. We calculate the usize of the set of accepting states.
        let accepting_states_mask = subset_as_usize(&states, &accepting_states);

        for state in deter_states.into_iter() {
            if state & accepting_states_mask != 0 {
                deter.accepting_states.insert(state);
            }
        }

        deter
    }
}

impl<S, A> ops::Not for Deter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Output = Deter<S, A>;

    fn not(self) -> Deter<S, A> {
        self.complement()
    }
}

impl<'a, 'b, S, T, A> ops::BitOr<&'b Deter<T, A>> for &'a Deter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Output = Deter<(S, T), A>;

    fn bitor(self, rhs: &Deter<T, A>) -> Deter<(S, T), A> {
        self.union(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::BitAnd<&'b Deter<T, A>> for &'a Deter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Output = Deter<(S, T), A>;

    fn bitand(self, rhs: &Deter<T, A>) -> Deter<(S, T), A> {
        self.intersection(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::BitXor<&'b Deter<T, A>> for &'a Deter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Output = Deter<(S, T), A>;

    fn bitxor(self, rhs: &Deter<T, A>) -> Deter<(S, T), A> {
        self.symmetric_difference(rhs)
    }
}

impl<'a, 'b, S, T, A> ops::Sub<&'b Deter<T, A>> for &'a Deter<S, A>
where
    S: Clone + Eq + Hash + ::std::fmt::Debug,
    T: Clone + Eq + Hash + ::std::fmt::Debug,
    A: Clone + Eq + Hash + ::std::fmt::Debug,
{
    type Output = Deter<(S, T), A>;

    fn sub(self, rhs: &Deter<T, A>) -> Deter<(S, T), A> {
        self.difference(rhs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_1() {
        let deter = Deter {
            initial_state: 'a',
            accepting_states: vec!['c', 'd', 'e'].into_iter().collect(),
            transitions: vec![
                (('a', 0), 'b'),
                (('a', 1), 'c'),
                (('b', 0), 'a'),
                (('b', 1), 'd'),
                (('c', 0), 'e'),
                (('c', 1), 'f'),
                (('d', 0), 'e'),
                (('d', 1), 'f'),
                (('e', 0), 'e'),
                (('e', 1), 'f'),
                (('f', 0), 'f'),
                (('f', 1), 'f'),
            ].into_iter()
                .collect(),
        };

        let minimal = deter.minimal();

        println!("{:?}", minimal);

        assert!(minimal.accepts(vec![0, 0, 0, 1, 0, 0]));
        assert!(minimal.accepts(vec![1, 0]));
        assert!(!minimal.accepts(vec![1, 0, 1]));
        assert!(!minimal.accepts(vec![]));
    }
}
