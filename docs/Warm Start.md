---
output:
  pdf_document: default
  html_document: default
---
# Warm Start Documentation

The following document describes the warm start configuration file. The contents 
of this file will define how treatments are assigned at the onset of the trial
to be used as a "warm start" for the RL algorithm.

## File Format 
For the moment, the file must be a yaml file. For more information on yaml please
view the following [link](https://yaml.org/).


## File Structure
The yaml file allows for a nested structure through which one can define a sequence 
of clauses as well as a sequence of sub clauses within each clause.

At the top level of the hierarchy there will be an **actions** component followed 
by **clause** components. The actions component must be titled actions and the 
clause component must be titled clause i, where i indexes the clauses.

For example, a simple yml warm start may look like the following:

```yml
actions: [a1, a2, a3]
clause 1: ...
clause 2: ...
```

The action component contains an array of all the available actions during the trial.
The clause components contain further nested yml structure that is explained in the 
following section.

### Clause components

