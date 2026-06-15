library(here)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
graph<-grViz("
digraph replication_tree {

  graph [layout = dot, rankdir = TB, fontname = 'Helvetica', 
         bgcolor = white, splines = ortho]
  
  node [fontname = 'Helvetica', fontsize = 11, style = filled, 
        shape = rectangle, margin = '0.2,0.1']
  
  edge [fontname = 'Helvetica', fontsize = 9, color = '#888888']

  # ── Level 0: Population ──
  pop [label = 'Fixed synthetic population\nN = 50,000, generated once', 
       fillcolor = '#E8E8E8', color = '#888888']

  # ── Level 1: Sample ──
  samp [label = 'Draw sample\nn = 2,000  |  W_i ~ Bernoulli(0.5)\nObserve Y_i, Y_i(W_i)', 
        fillcolor = '#E8E8E8', color = '#888888']

  # ── Level 2: Missingness worlds ──
  m0 [label = 'm0: 0%\ncomplete data', 
      fillcolor = '#D4EDDA', color = '#5A9A6A']
  m1 [label = 'm1: 10% MAR\nQUOTE_VALUE + RFM_SEGMENT', 
      fillcolor = '#FFF3CD', color = '#B8860B']
  m2 [label = 'm2: 50% MAR\nQUOTE_VALUE + RFM_SEGMENT', 
      fillcolor = '#FFF3CD', color = '#B8860B']
  m3 [label = 'm3: 70% MAR\nQUOTE_VALUE + RFM_SEGMENT', 
      fillcolor = '#FFF3CD', color = '#B8860B']

  # ── Level 3: Imputation methods ──
  # m0 gets only CCA (no missingness)
  m0_cca [label = 'CCA\n(NO IMPUTATION)', 
           fillcolor = '#E8E0F0', color = '#7B5EA7']

  # m1 gets 3 methods
  m1_cca  [label = 'CCA',       fillcolor = '#E8E0F0', color = '#7B5EA7']
  m1_mm   [label = 'Mean/mode', fillcolor = '#E8E0F0', color = '#7B5EA7']
  m1_mice [label = 'MICE\nm=5', fillcolor = '#E8E0F0', color = '#7B5EA7']

  # m2 gets 3 methods
  m2_cca  [label = 'CCA',       fillcolor = '#E8E0F0', color = '#7B5EA7']
  m2_mm   [label = 'Mean/mode', fillcolor = '#E8E0F0', color = '#7B5EA7']
  m2_mice [label = 'MICE\nm=5', fillcolor = '#E8E0F0', color = '#7B5EA7']

  # m3 gets 3 methods
  m3_cca  [label = 'CCA',       fillcolor = '#E8E0F0', color = '#7B5EA7']
  m3_mm   [label = 'Mean/mode', fillcolor = '#E8E0F0', color = '#7B5EA7']
  m3_mice [label = 'MICE\nm=5', fillcolor = '#E8E0F0', color = '#7B5EA7']

  # ── Level 4: Estimators (one set per imputation node) ──
  node [fillcolor = '#FADADD', color = '#C0555A', fontsize = 9, 
        margin = '0.1,0.08']

  # m0/CCA
  m0c_s [label = 'S-learner']
  m0c_t [label = 'T-learner']
  m0c_x [label = 'X-learner']
  m0c_cf[label = 'Causal\nForest']

  # m1/CCA
  m1c_s [label = 'S-learner']
  m1c_t [label = 'T-learner']
  m1c_x [label = 'X-learner']
  m1c_cf[label = 'Causal\nForest']

  # m1/mean
  m1m_s [label = 'S-learner']
  m1m_t [label = 'T-learner']
  m1m_x [label = 'X-learner']
  m1m_cf[label = 'Causal\nForest']

  # m1/mice
  m1i_s [label = 'S-learner']
  m1i_t [label = 'T-learner']
  m1i_x [label = 'X-learner']
  m1i_cf[label = 'Causal\nForest']

  # m2/CCA
  m2c_s [label = 'S-learner']
  m2c_t [label = 'T-learner']
  m2c_x [label = 'X-learner']
  m2c_cf[label = 'Causal\nForest']

  # m2/mean
  m2m_s [label = 'S-learner']
  m2m_t [label = 'T-learner']
  m2m_x [label = 'X-learner']
  m2m_cf[label = 'Causal\nForest']

  # m2/mice
  m2i_s [label = 'S-learner']
  m2i_t [label = 'T-learner']
  m2i_x [label = 'X-learner']
  m2i_cf[label = 'Causal\nForest']

  # m3/CCA
  m3c_s [label = 'S-learner']
  m3c_t [label = 'T-learner']
  m3c_x [label = 'X-learner']
  m3c_cf[label = 'Causal\nForest']

  # m3/mean
  m3m_s [label = 'S-learner']
  m3m_t [label = 'T-learner']
  m3m_x [label = 'X-learner']
  m3m_cf[label = 'Causal\nForest']

  # m3/mice
  m3i_s [label = 'S-learner']
  m3i_t [label = 'T-learner']
  m3i_x [label = 'X-learner']
  m3i_cf[label = 'Causal\nForest']

  # ── Level 5: Output ──
  node [shape = rectangle, fontsize = 11, margin = '0.2,0.1',
        fillcolor = '#D4EDDA', color = '#5A9A6A']

  out [label = '40 CATE estimate vectors per replication \nRepeated R = 1,000 times']

  # ── Edges: top levels ──
  pop  -> samp
  samp -> {m0 m1 m2 m3}

  # missingness -> imputation
  m0 -> m0_cca
  m1 -> {m1_cca m1_mm m1_mice}
  m2 -> {m2_cca m2_mm m2_mice}
  m3 -> {m3_cca m3_mm m3_mice}

  # imputation -> estimators
  m0_cca  -> {m0c_s  m0c_t  m0c_x  m0c_cf}
  m1_cca  -> {m1c_s  m1c_t  m1c_x  m1c_cf}
  m1_mm   -> {m1m_s  m1m_t  m1m_x  m1m_cf}
  m1_mice -> {m1i_s  m1i_t  m1i_x  m1i_cf}
  m2_cca  -> {m2c_s  m2c_t  m2c_x  m2c_cf}
  m2_mm   -> {m2m_s  m2m_t  m2m_x  m2m_cf}
  m2_mice -> {m2i_s  m2i_t  m2i_x  m2i_cf}
  m3_cca  -> {m3c_s  m3c_t  m3c_x  m3c_cf}
  m3_mm   -> {m3m_s  m3m_t  m3m_x  m3m_cf}
  m3_mice -> {m3i_s  m3i_t  m3i_x  m3i_cf}

  # estimators -> output
  {m0c_s  m0c_t  m0c_x  m0c_cf
   m1c_s  m1c_t  m1c_x  m1c_cf
   m1m_s  m1m_t  m1m_x  m1m_cf
   m1i_s  m1i_t  m1i_x  m1i_cf
   m2c_s  m2c_t  m2c_x  m2c_cf
   m2m_s  m2m_t  m2m_x  m2m_cf
   m2i_s  m2i_t  m2i_x  m2i_cf
   m3c_s  m3c_t  m3c_x  m3c_cf
   m3m_s  m3m_t  m3m_x  m3m_cf
   m3i_s  m3i_t  m3i_x  m3i_cf} -> out

  # ── Rank groupings to enforce horizontal alignment ──
  {rank = same; m0; m1; m2; m3}
  {rank = same; m0_cca; m1_cca; m1_mm; m1_mice; 
                m2_cca; m2_mm; m2_mice;
                m3_cca; m3_mm; m3_mice}
  {rank = same; m0c_s; m0c_t; m0c_x; m0c_cf;
                m1c_s; m1c_t; m1c_x; m1c_cf;
                m1m_s; m1m_t; m1m_x; m1m_cf;
                m1i_s; m1i_t; m1i_x; m1i_cf;
                m2c_s; m2c_t; m2c_x; m2c_cf;
                m2m_s; m2m_t; m2m_x; m2m_cf;
                m2i_s; m2i_t; m2i_x; m2i_cf;
                m3c_s; m3c_t; m3c_x; m3c_cf;
                m3m_s; m3m_t; m3m_x; m3m_cf;
                m3i_s; m3i_t; m3i_x; m3i_cf}
}
")
svg_file <- tempfile(fileext = ".svg")
writeLines(export_svg(graph), svg_file)
rsvg_png(svg_file, here("output", "replication_tree.png"), width = 2400)
