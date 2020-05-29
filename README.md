# Social-Network-Analytics
Analysis of various networks

This repo contains all of the work I completed in my Social Networks Analytics course. There were 5 challenging exercises that I had to complete, each about a different network system to be analyzed. I implemented all of this in R, and will list the subject of each file below:

Social_clique.Rmd: This is a classroom system of two networks: one for social ties, and the other for working ties. They detail the social dynamics within the classroom. The social ties are about how students interact with each other, and how that network of friendship is structured. On the other hand, the task ties has to do with which students work together to get assignments accomplished. In this assignment, I calculate network centrality statistics, transitivity, betweenness, minimum walks, and more.

venture_capital.R: This is a network of venture capitalist firms. I complete an analysis to determine if venture capital firms that are highly centralized within the network have a better success rate and profits as opposed to less-connected venture capital firms. In other words, I am trying to determine if firms that are part of the core of connections perform better than those on the periphery.

indian_election.R: This is an interesting application of network analytics, where we treat the web of Indian states/districts as a network. States that are geographically close to each other are neighbors and considered to have a tie. We use an analysis of this network, along with weather and party data, to see network effects on the spreading of political parties. Does the rise of a political party in one node/state lead to the party's spread to other neighboring states?

movie_companies.R: This is an analysis of movie production companies. There are a few networks to analyze here. The first is the network of production companies that work together to produce movies. The second is the network of actors/actresses and staff members that combine their efforts to produce a movie. This analysis looks at the genres and ventures that each production company gets involved in, to determine whether casting a wide net of genres yields better success. Furthermore, it explores whether newer, innovative movie topics yields greater success versus movies that are more in tune with the times or have been done before. The analysis takes into account the revolving door of actors/actresses/staff, and tries to isolate the effect of making innovative movies on production company success.
