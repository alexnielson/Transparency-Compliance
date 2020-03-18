# Transparency Compliance Enforcement Flowchart                             ####

# Written by Michael Jensen.

# Purpose: Create a flowchart that outlines the process for enforcing
# compliance with Transparency reporting requirements.

#   ____________________________________________________________________________
#   Define Plot and Coordinates                                             ####
library(diagram)

openplotmat(main = "Transparency Compliance Enforcement")
pos <- coordinates(c(1, 9, 9, 9, 9, 9, 9, 9, 9, 9, 1))

#   ____________________________________________________________________________
#   Row 1: Data submitted?                                                  ####
textrect(pos[1, ], 0.135, 0.023, shadow.size = 0, 
         lab = "Required data submitted?", 
         box.col = "white", lcol = "black")

#   ____________________________________________________________________________
#   Row 2: Yes or No                                                        ####
textrect(pos[2, ], 0.03, 0.023, shadow.size = 0, 
         lab = "Yes", 
         box.col = "white")

textrect(pos[8, ], 0.03, 0.023, shadow.size = 0, 
         lab = "No", 
         box.col = "white")

straightarrow(from = pos[1, ], to = pos[2, ] + c(0, 0.015), 
              lwd = 1, arr.pos = 0.9,
              segment = c(0.31, 0.9), arr.length = 0.3)

straightarrow(from = pos[1, ] + c(0.1, 0), to = pos[8, ], 
              lwd = 1, arr.pos = 0.69, 
              segment = c(0.3, 0.69), arr.length = 0.3)

#   ____________________________________________________________________________
#   Row 3: Question                                                         ####
textrect(pos[17, ], 0.18, 0.023, shadow.size = 0, 
         lab = "Entity noticed of missing period(s)?", 
         box.col = "white")

straightarrow(from = pos[8, ], to = pos[17, ], 
              lwd = 1, arr.pos = 0.55,
              segment = c(0.3, 0.55), arr.length = 0.3)

#   ____________________________________________________________________________
#   Row 4: Yes or No                                                        ####
textrect(pos[23, ], 0.03, 0.023, shadow.size = 0, 
         lab = "Yes", 
         box.col = "white")

textrect(pos[27, ], 0.03, 0.023, shadow.size = 0, 
         lab = "No", 
         box.col = "white")

straightarrow(from = pos[17, ] + c(-0.11, 0), to = pos[23, ], 
              lwd = 1, arr.pos = 0.81,
              segment = c(0.3, 0.81), arr.length = 0.3)

straightarrow(from = pos[17, ], to = pos[27, ], 
              lwd = 1, arr.pos = 0.64,
              segment = c(0.3, 0.64), arr.length = 0.3)

#   ____________________________________________________________________________
#   Row 5: Questions                                                        ####
textrect(pos[32, ], 0.15, 0.023, shadow.size = 0,
         lab = "60-day grace period expired?", 
         box.col = "white", lcol = "black")

textrect(pos[36, ], 0.11, 0.023, shadow.size = 0,
         lab = "Enforce compliance?", 
         box.col = "white", lcol = "black")

straightarrow(from = pos[23, ], to = pos[32, ], 
              lwd = 1, arr.pos = 0.55,
              segment = c(0.3, 0.55), arr.length = 0.3)

straightarrow(from = pos[27, ], to = pos[36, ], 
              lwd = 1, arr.pos = 0.55,
              segment = c(0.3, 0.55), arr.length = 0.3)

#   ____________________________________________________________________________
#   Row 6: Yes or No                                                        ####
textrect(pos[40, ], 0.03, 0.023, shadow.size = 0, 
         lab = "Yes", 
         box.col = "white")

textrect(pos[42, ], 0.03, 0.023, shadow.size = 0, 
         lab = "No", 
         box.col = "white")

textrect(pos[44, ], 0.03, 0.023, shadow.size = 0, 
         lab = "Yes", 
         box.col = "white")

textrect(pos[46, ], 0.03, 0.023, shadow.size = 0, 
         lab = "No", 
         box.col = "white")

straightarrow(from = pos[32, ], to = pos[40, ], 
              lwd = 1, arr.pos = 0.6,
              segment = c(0.3, 0.6), arr.length = 0.3)

straightarrow(from = pos[32, ], to = pos[42, ], 
              lwd = 1, arr.pos = 0.6,
              segment = c(0.3, 0.6), arr.length = 0.3)

straightarrow(from = pos[36, ], to = pos[44, ], 
              lwd = 1, arr.pos = 0.6,
              segment = c(0.3, 0.6), arr.length = 0.3)

straightarrow(from = pos[36, ], to = pos[46, ], 
              lwd = 1, arr.pos = 0.6,
              segment = c(0.3, 0.6), arr.length = 0.3)

#   ____________________________________________________________________________
#   Row 7: Uploaded file?                                                   ####
textrect(pos[49, ], 0.075, 0.023, shadow.size = 0, 
         lab = "Uploaded file?", 
         box.col = "white")

textrect(pos[53, ], 0.075, 0.023, shadow.size = 0, 
         lab = "Uploaded file?", 
         box.col = "white")

straightarrow(from = pos[40, ], to = pos[49, ], 
              lwd = 1, arr.pos = 0.55,
              segment = c(0.3, 0.55), arr.length = 0.3)

straightarrow(from = pos[44, ], to = pos[53, ], 
              lwd = 1, arr.pos = 0.55,
              segment = c(0.3, 0.55), arr.length = 0.3)

#   ____________________________________________________________________________
#   Row 8: Yes/No                                                           ####
textrect(pos[57, ], 0.03, 0.023, shadow.size = 0, 
         lab = "Yes", 
         box.col = "white")

textrect(pos[59, ], 0.03, 0.023, shadow.size = 0, 
         lab = "No", 
         box.col = "white")

textrect(pos[61, ], 0.03, 0.023, shadow.size = 0, 
         lab = "Yes", 
         box.col = "white")

textrect(pos[63, ], 0.03, 0.023, shadow.size = 0, 
         lab = "No", 
         box.col = "white")

straightarrow(from = pos[49, ], to = pos[57, ], 
              lwd = 1, arr.pos = 0.6,
              segment = c(0.3, 0.6), arr.length = 0.3)

straightarrow(from = pos[49, ], to = pos[59, ], 
              lwd = 1, arr.pos = 0.6,
              segment = c(0.3, 0.6), arr.length = 0.3)

straightarrow(from = pos[53, ], to = pos[61, ], 
              lwd = 1, arr.pos = 0.6,
              segment = c(0.3, 0.6), arr.length = 0.3)

straightarrow(from = pos[53, ], to = pos[63, ], 
              lwd = 1, arr.pos = 0.6,
              segment = c(0.3, 0.6), arr.length = 0.3)

#   ____________________________________________________________________________
#   Row 9: Entity Status                                                    ####
textrect(pos[65, ], 0.045, 0.023, shadow.size = 0,
         lab = "Current", 
         box.col = "forestgreen")

textrect(pos[66, ], 0.045, 0.023, shadow.size = 0, 
         lab = "Current", 
         box.col = "forestgreen")

textrect(pos[68, ], 0.054, 0.023, shadow.size = 0, 
         lab = "Delinquent", 
         box.col = "red3")

textrect(pos[69, ], 0.054, 0.023, shadow.size = 0, 
         lab = "Delinquent", 
         box.col = "red3")

textrect(pos[70, ], 0.045, 0.023, shadow.size = 0, 
         lab = "Current", 
         box.col = "forestgreen")

textrect(pos[72, ], 0.054, 0.023, shadow.size = 0, 
         lab = "Delinquent", 
         box.col = "red3")

textrect(pos[73, ], 0.045, 0.023, shadow.size = 0, 
         lab = "Missing", 
         box.col = "gold")

straightarrow(from = pos[2, ], to = pos[65, ], 
              lwd = 1, arr.pos = 0.93,
              segment = c(0.05, 0.93), arr.length = 0.3)

straightarrow(from = pos[57, ], to = pos[66, ], 
              lwd = 1, arr.pos = 0.55,
              segment = c(0.3, 0.55), arr.length = 0.3)

straightarrow(from = pos[59, ], to = pos[68, ], 
              lwd = 1, arr.pos = 0.55,
              segment = c(0.3, 0.55), arr.length = 0.3)

straightarrow(from = pos[42, ], to = pos[69, ], 
              lwd = 1, arr.pos = 0.85,
              segment = c(0.1, 0.85), arr.length = 0.3)

straightarrow(from = pos[61, ], to = pos[70, ], 
              lwd = 1, arr.pos = 0.55,
              segment = c(0.3, 0.55), arr.length = 0.3)

straightarrow(from = pos[63, ], to = pos[72, ], 
              lwd = 1, arr.pos = 0.55,
              segment = c(0.3, 0.55), arr.length = 0.3)

straightarrow(from = pos[46, ], to = pos[73, ], 
              lwd = 1, arr.pos = 0.85,
              segment = c(0.1, 0.85), arr.length = 0.3)

#   ____________________________________________________________________________
#   Row 10: Action                                                          ####
textrect(pos[77, ], 0.056, 0.023, shadow.size = 0, 
         lab = "Hold funds", 
         box.col = "dodgerblue1")

textrect(pos[81, ], 0.07, 0.023, shadow.size = 0, 
         lab = "60-day notice", 
         box.col = "dodgerblue1")

straightarrow(from = pos[68, ], to = pos[77, ], 
              lwd = 1, arr.pos = 0.55,
              segment = c(0.3, 0.55), arr.length = 0.3)

straightarrow(from = pos[72, ], to = pos[81, ], 
              lwd = 1, arr.pos = 0.55,
              segment = c(0.3, 0.55), arr.length = 0.3)

#   ____________________________________________________________________________
#   Row 11: Update on next check                                            ####
textrect(pos[83, ], 0.22, 0.023, shadow.size = 0, 
         lab = "Update status on next compliance check", 
         box.col = "white")

bentarrow(from = pos[65, ] + c(0, -0.025), to = pos[83, ], 
          lwd = 1, arr.pos = 0.47,
          segment = c(0, 0.47), arr.length = 0.3, path = "V")

straightarrow(from = pos[66, ] + c(-0.04, 0), to = pos[83, ] + c(-0.196, 0), 
              lwd = 1, arr.pos = 0.81,
              segment = c(0.14, 0.81), arr.length = 0.3)

straightarrow(from = pos[77, ], to = pos[83, ] + c(-0.1115, 0), 
              lwd = 1, arr.pos = 0.58,
              segment = c(0.26, 0.58), arr.length = 0.3)

straightarrow(from = pos[69, ], to = pos[83, ], 
              lwd = 1, arr.pos = 0.78,
              segment = c(0.14, 0.78), arr.length = 0.3)

straightarrow(from = pos[70, ], to = pos[83, ] + c(0.11, 0), 
              lwd = 1, arr.pos = 0.78,
              segment = c(0.15, 0.78), arr.length = 0.3)

straightarrow(from = pos[81, ] + c(0.08, 0), to = pos[83, ] + c(0.15, 0), 
              lwd = 1, arr.pos = 0.69,
              segment = c(0.26, 0.69), arr.length = 0.3)

bentarrow(from = pos[73, ] + c(0, -0.025), to = pos[83, ], 
          lwd = 1, arr.pos = 0.47,
          segment = c(0, 0.47), arr.length = 0.3, path = "V")

