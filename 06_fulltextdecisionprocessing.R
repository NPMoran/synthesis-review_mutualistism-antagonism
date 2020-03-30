##########_____Mutualism/Antagonism Synthesis Review_____#########


Sys.setenv(LANG = "en")
library(dplyr); library(operators)


#Processing of Fulltext response via the google form
MA.fulltextscreening_responses <- read.csv("./MA.fulltextscreening.responses30032020.csv", strip.white = TRUE)

nrow(MA.fulltextscreening_responses)
labels(MA.fulltextscreening_responses)
MA.fulltextscreening_responses <- select(MA.fulltextscreening_responses, -c(X, X.1, X.2, X.3, X.4, X.5, X.6))
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, abstract.id = Q1..Abstract.ID..MA1..MA2.etc..)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, FulltextReviewer = Q2..Screener.ID)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, StudyType = Q3..Study.Type)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, InteractionType = Q4..Interaction.type)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, Scale = Q5..Ecological.scale.of.the.interaction)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, SpeciesInteracting = Q6..Interacting.species..scientific.name.s.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, TraitDescription = Q7..What.trait.influences.the.quality.of.the.interaction...e.g..behavioural.phenotype.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, SpeciesTraitVariation = Q8..In.which.species.does.the.trait.vary.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, GeneticNotes = Q9..Is.anything.know.about.the.genetic.basis.or.heritability.of.the.trait...focusing.on.information.provided.by.the.paper.only.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, Is.there.a.shift.in.the.interaction = Q10..Does.the.study.show.a.shift.in.the.interaction.between.mutualism.antagonism.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, What.is.the.scale.of.the.shift = Q11..In.what.scale.does.the.interaction.shift.between.mutualistic.and.antagonistic..e.g..does.the.trait.variation.lead.to.individuals..populations.or.species.varying.along.this.continuum.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, What.is.the.direction.of.shift = Q12..What.is.the.direction.of.shift.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, What.factors.are.driving.that.shift = Q13..What.factors.are.driving.that.shift...e.g..ecological..abiotic..social.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, What.is.the.ecological.impact.of.the.interaction.and.shift.of.that.interaction = Q14..What.is.the.ecological.impact.of.the.interaction.and.shift.of.that.interaction...i.e..what.ecological.processes.may.be.effected..focusing.on.information.provided.by.the.paper.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, Is.there.a.connection.to.the.NC3 = Q15..Is.there.a.connection.to.the.NC3...e.g..the.promotion.and.maintenance.of.individual.level.traits.or.niche.variation.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, Reviewer.outstanding.concerns = Q16..Do.you.have.any.outstanding.concerns.or.questions.about.the.paper..e.g..is.there.no.clear.trait.that.appears.to.vary..is.there.no.clear.shift.in.the.interaction.between.mutualism.and.antagonism.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, Reviewer.notes = Q17..Additional.notes..include.any.points.of.significance.that.are.not.included.in.this.questionnaire.)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, ReviewerDecision = Q18..Do.you.think.this.paper.can.be.included.within.the.review..on.the.basis.of.our.inclusion.exclusion.criteria..)
MA.fulltextscreening_responses <- rename(MA.fulltextscreening_responses, ReviewerDecisionReason = Q19..If.No.Maybe..why.)


#Removing some irrelevant responses 
MA.fulltextscreening_responses <- subset(MA.fulltextscreening_responses, abstract.id != "TEST")
MA.fulltextscreening_responses <- subset(MA.fulltextscreening_responses, Timestamp != "10/29/2019 0:30:18") #removing JW form testing submission
MA.fulltextscreening_responses <- subset(MA.fulltextscreening_responses, Timestamp != "1/13/2020 5:05:35") #NL reviewed MA91 twice, second review taken as their final decision


#Producing a database for final decisions
MA.fulltextscreening.allocations <- read.csv("MA.fulltextscreening.allocations.csv")
labels(MA.fulltextscreening.allocations)
MA.fulltextscreening.allocations <- select(MA.fulltextscreening.allocations, -c(X, X.1, X.2, X.3, X.4, X.5))
MA.fulltextscreening.allocations <- rename(MA.fulltextscreening.allocations, FulltextReviewer.initial = FulltextReviewer)

MA.fulltextscreening_responses <- merge(MA.fulltextscreening.allocations, MA.fulltextscreening_responses, by = "abstract.id", all.x = TRUE)
MA.fulltextscreening_responses$ReviewerDecision.checked.by <- ""
MA.fulltextscreening_responses$FinalDecision <- ""
MA.fulltextscreening_responses$FinalDecisionReason <- ""
write.csv(MA.fulltextscreening_responses, "MA.fulltextscreening.responses.csv")
subset(MA.fulltextscreening_responses, abstract.id == "MA14"
       