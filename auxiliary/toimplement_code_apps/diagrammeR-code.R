library(DiagrammeR)

DiagrammeR("digraph SEIR {
  graph [rankdir=TD, overlap=false, fontsize = 10]
    node[shape=oval, label='S'] S;
    node[shape=oval, label='E'] E;
    node[shape=oval, label='I'] I;
    node[shape=oval, label='R'] R;
    S->E E->I I->R
  {S E I R}
}",type="grViz",engine="dot",height=300,width=800)
