# Transparency Process 

# Code author: Michael Jensen.

# Code purpose: Create a flowchart that outlines the reports generated for 
# Transparency enforcement.

library(diagram)

openplotmat(main = "Transparency Reports Flowchart")
pos <- coordinates(c(1, 1, 4, 4, 4, 4, 4, 4, 4, 4))

textrect(pos[1, ], 0.17, 0.025, shadow.size = 0, lab = "SF Reports", 
         box.col = "white")

textrect(pos[2, ], 0.17, 0.025, shadow.size = 0, lab = "R", 
         box.col = "white")
straightarrow(from = pos[1, ], to = pos[2, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)

textrect(pos[7, ], 0.12, 0.055, shadow.size = 0, 
         lab = c("Report:", "Shared IDs"), box.col = "white")
textrect(pos[8, ], 0.12, 0.055, shadow.size = 0, 
         lab = c("Report:", "Unique IDs"), box.col = "white")
textrect(pos[9, ], 0.12, 0.055, shadow.size = 0, 
         lab = c("Report:", "Compliance"), box.col = "white")
textrect(pos[10, ], 0.12, 0.055, shadow.size = 0, 
         lab = c("Report:", "Reliability"), box.col = "white")
straightarrow(from = pos[2, ], to = pos[7, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)
straightarrow(from = pos[2, ], to = pos[8, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)
straightarrow(from = pos[2, ], to = pos[9, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)
straightarrow(from = pos[2, ], to = pos[10, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)

textrect(pos[15, ], 0.12, 0.055, shadow.size = 0, 
         lab = c("Excel:", "Fuzzy Match"), box.col = "white")
textrect(pos[16, ], 0.12, 0.055, shadow.size = 0, 
         lab = c("Update", "Salesforce"), box.col = "forestgreen")
textrect(pos[17, ], 0.12, 0.055, shadow.size = 0, 
         lab = c("Internal", "Check"), box.col = "white")
textrect(pos[18, ], 0.12, 0.055, shadow.size = 0, 
         lab = c("Enforcement", "Process"), box.col = "forestgreen")
straightarrow(from = pos[7, ], to = pos[15, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)
straightarrow(from = pos[8, ], to = pos[16, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)
straightarrow(from = pos[9, ], to = pos[17, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)
straightarrow(from = pos[10, ], to = pos[18, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)

textrect(pos[23, ], 0.12, 0.055, shadow.size = 0, 
         lab = c("Update", "Salesforce"), box.col = "forestgreen")
textrect(pos[25, ], 0.12, 0.055, shadow.size = 0, 
         lab = c("Darrell", "and John"), box.col = "white")
straightarrow(from = pos[15, ], to = pos[23, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)
straightarrow(from = pos[17, ], to = pos[25, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)

textrect(pos[33, ], 0.12, 0.055, shadow.size = 0, 
         lab = c("Enforcement", "Process"), box.col = "forestgreen")
straightarrow(from = pos[25, ], to = pos[33, ], lwd = 1, arr.pos = 0.65,
              segment = c(0.25, 0.75), arr.length = 0.3)

