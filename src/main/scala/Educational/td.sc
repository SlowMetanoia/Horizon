case class CourseEntity(id: Any,
                        name: String,
                        inputSkills: Any,
                        outputSkills: Any,
                        inputAbilities: Any,
                        outputAbilities: Any,
                        inputKnowledge: Any,
                        outputKnowledge: Any
                       )
/*
val c = for (i <- 1 until 10)
  yield CourseEntity(
       id = UUID.randomUUID(),
       name = s"Course$i",
       inputSkills = inputSkills,
       outputSkills = outputSkills,
       inputAbilities = inputAbilities,
       outputAbilities = outputAbilities,
       inputKnowledge = inputKnowledge,
       outputKnowledge = outputKnowledge
       )
*/
val d = for (i<-1 to 10) yield i*2