p_Igushik <- make_age(data_Igushik, 3, 8)
brood_Igushik <- make_brood(data_Igushik, p_Igushik)
goal_Igushik <-
  data.frame(
    yr = c(1984, 2001, 2015),
    lb = c(150000, 150000, 150000),
    ub = c(250000, 300000, 400000)
  )
goal_Igushik_new <-
  data.frame(
    yr = c(1984, 2001, 2015, 2026),
    lb = c(150000, 150000, 150000, 200000),
    ub = c(250000, 300000, 400000, 500000)
  )

# test plot_s w and wo a new goal finding
plot_escapement(brood_Igushik,
       goal_Igushik,
       "Igushik River Sockeye Salmon"
)
plot_escapement(brood_Igushik,
       goal_Igushik_new,
       "Igushik River Sockeye Salmon"
)

#test plot_SR function
# No goal change
plot_SR(post_Igushik_byr63_15,
        brood_Igushik,
        goal_dat = goal_Igushik,
        "Igushik River Sockeye Salmon",
        multiplier = 1e-5)
#new finding
plot_SR(post_Igushik_byr63_15,
        brood_Igushik,
        goal_dat = goal_Igushik_new,
        "Igushik River Sockeye Salmon",
        multiplier = 1e-5)
#first finding
plot_SR(post_Igushik_byr63_15,
        brood_Igushik,
        goal_dat = goal_Igushik_new[goal_Igushik_new$yr == 2026, ],
        "Igushik River Sockeye Salmon",
        multiplier = 1e-5)

# Create profiles for plot_ey and plot_profil testing
post_list <-
  list(
    'byr: 1963-2005' = post_Igushik_byr63_05,
    'byr: 1963-2017' = post_Igushik_byr63_15
  )
profile_list <- lapply(post_list, get_profile, multiplier = 1e-5)
profile_list80 <- lapply(post_list, get_profile, multiplier = 1e-5, MSY_pct = 80)

#test plot_ey
# No goal change
plot_ey(profile_list[[2]],
        brood_Igushik,
        goal_dat = goal_Igushik,
        "Igushik River Sockeye Salmon"
)
#new finding
plot_ey(profile_list[[2]],
        brood_Igushik,
        goal_dat = goal_Igushik_new,
        "Igushik River Sockeye Salmon")
#first finding
plot_ey(profile_list[[2]],
        brood_Igushik,
        goal_dat = goal_Igushik_new[goal_Igushik_new$yr == 2026, ],
        "Igushik River Sockeye Salmon")

#test plot_profile
# no goal change
plot_profile(profile_list[[2]],
             goal_Igushik,
             "Igushik River Sockeye Salmon")
plot_profile(profile_list80[[2]],
             goal_Igushik,
             "Igushik River Sockeye Salmon")
# new finding
plot_profile(profile_list[[2]],
             goal_Igushik_new,
             "Igushik River Sockeye Salmon",
             )
plot_profile(profile_list80[[2]],
             goal_Igushik_new,
             "Igushik River Sockeye Salmon",
)
# first finding
plot_profile(profile_list[[2]],
             goal_Igushik_new[goal_Igushik_new$yr == 2026, ],
             "Igushik River Sockeye Salmon")
plot_profile(profile_list80[[2]],
             goal_Igushik_new[goal_Igushik_new$yr == 2026, ],
             "Igushik River Sockeye Salmon")

#test plot_profile2
# no goal change
plot_profile_facet(profile_list,
             goal_Igushik,
             "Igushik River Sockeye Salmon")
plot_profile_facet(profile_list80,
             goal_Igushik,
             "Igushik River Sockeye Salmon")
# new finding
plot_profile_facet(profile_list,
             goal_Igushik_new,
             "Igushik River Sockeye Salmon",
)
plot_profile_facet(profile_list80,
             goal_Igushik_new,
             "Igushik River Sockeye Salmon",
)
# first finding
# These should not work, and don't. There would not be 2 profiles for a first finding.
plot_profile_facet(profile_list,
             goal_Igushik_new[goal_Igushik_new$yr == "new", ],
             "Igushik River Sockeye Salmon")
plot_profile_facet(profile_list80,
             goal_Igushik_new[goal_Igushik_new$yr == "new", ],
             "Igushik River Sockeye Salmon")

# Test EGoutput
# 90% of MSY
output_SR(post_list, brood_Igushik, goal_Igushik, "Igushik River Sockeye Salmon", multiplier = 1e-5)
output_SR(post_list[[2]], brood_Igushik, goal_Igushik, "Igushik River Sockeye Salmon", multiplier = 1e-5)

# 80% & 90% of MSY
output_SR(post_list, brood_Igushik, goal_Igushik, "Igushik River Sockeye Salmon", MSY_pct = 80, multiplier = 1e-5)
output_SR(post_list[[2]], brood_Igushik, goal_Igushik, "Igushik River Sockeye Salmon", MSY_pct = 80, multiplier = 1e-5)
