1.0.0
-----
* First version

2.0.0
-----
* Upgrade text-builder to 1.0

3.0.0
-----
* Add node anchoring: edges can be created by referring to an entity's name, even if that name hasn't been registered yet, allowing for cycles and reducing the amount of manual bookkeeping
* Remove `subgraphWith` and `clusterWith` functions, as the subgraph or cluster's entity is always accessible via `its` or `itsID`
* Resolve `lhead` and `ltail` as late possible, fixing a rare edge case where an edge pointing to a cluster would not set the attributes properly.
