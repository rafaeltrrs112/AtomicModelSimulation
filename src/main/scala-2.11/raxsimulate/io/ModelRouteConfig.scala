package raxsimulate.io

import raxsimulate.model.Model

/**
 * Routed Models must be inserted into a router in the order of their output index
 * out of the router. I.E. if added first with inputIndices(0,1,2) inputs I(0,1,2) -> O(0)
 * @param model
 *              The model for routing.
 * @param inputIndices
 *                     The indices of the inputs.
 */
case class ModelRouteConfig(model : Model, inputIndices : IndexedSeq[Int], isOutputModel : Boolean = false)
