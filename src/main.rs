#![allow(unused_imports, unused_variables)]

extern crate automaton;

use automaton::{Deter, NonDeter};

fn main() {
    let deter = Deter {
        initial: 'a',
        accepting: vec!['c', 'd', 'e'].into_iter().collect(),
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

    println!("{:?}\n", deter);
    println!("{:?}\n", deter.reachable_states());
    println!("{:?}\n", deter.similar());
    println!("{:?}\n", deter.minimal());
}
