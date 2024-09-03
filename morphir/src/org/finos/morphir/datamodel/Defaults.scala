// package org.finos.morphir.datamodel
// import zio.*

// object MDMDefaults extends DefaultFiller {

//   def fillWithDefaults(data: Data, concept: Concept): DefaultTask =
//     (data, concept) match {
//       case (otherData, otherConcept) if otherData.shape == otherConcept =>
//         Right(otherData) // Hopefully == is good enough
//       case (recordData: Data.Record, recordConcept: Concept.Record) => for {
//           dataFields <- Defaults.collectAll(
//             recordConcept.fields,
//             { case (label, fieldConcept) =>
//               recordData.values.find(_._1 == label) match {
//                 case None                 => default(fieldConcept).map((label, _))
//                 case Some((_, fieldData)) => fillWithDefaults(fieldData, fieldConcept).map((label, _))
//               }
//             }
//           )
//           res = Data.Record(dataFields, recordConcept)
//         } yield res
//       // TODO: Other cases
//       case (otherData, otherConcept) => Left(Unfillable(otherData, otherConcept))
//     }

// }
