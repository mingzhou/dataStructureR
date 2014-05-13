#' ADT: Tree
#'

env.tree  <- new.env(parent = emptyenv())
.env.tree <- new.env(parent = emptyenv())

#' @export
#' 
tree.new.node <- function(node, children = NULL) {
  list(Node = node, Children = children)
}

tree.node <- function(t, nodeName) {
  appFunc <- function(node) {
    nodeName == node$Node$Name
  }
  
  aggFunc <- function(node, children) {
    tree.new.node(node, children)
  }
  
  tree.apply(t, appFunc, aggFunc)
}

#' @export
#' apply function to the specific node in the tree
tree.node.apply <- function(node, FUNC, ...) {
  FUNC(node$Node, ...)
}

#' @export
#' for product$stock$account
#' INPUT :
#'   -- appFunc :
#'       apply on each node
#'   -- aggFunc :
#'       aggregate outputs of appFunc
#' TODO : 1. curried function? Haskell
#'        2. bind function and arguments
#"        tree.apply <- function(t, appFunc = print, ..., aggFunc, ...) {
tree.apply <- function(t, appFunc, aggFunc) {
  # apply to node
  node <- tree.node.apply(t, appFunc)
  # apply to children
  children <- NULL
  if (length(t$Children) != 0) {
    children <- lapply(t$Children, tree.apply, appFunc, aggFunc)
  }
  aggFunc(node, children)
}

# #' @export
# #' for product$stock$account
# tree.apply <- function(t, FUNC = print, ...) {
#   # apply to node
#   tree.node.apply(t, FUNC, ...)
#   # apply to children
#   if (length(t$Children) != 0) {
#     children <- names(t$Children)
#     # length(columnNames) is very small, for-loop is convenient
#     for (child in children) {
#       tree.apply(t$Children[[child]], FUNC, ...)
#     }
#   }
# }

tree.child <- function(t, childName) {
  t$Children[[childName]]
}

tree.child.func <- function(t, childName, FUNC) {
  child <- t$Children[[childName]]
  
}

#' @export
#' tree structrue
tree1.identical <- function(t1, t2) {
  
}

# #' @export
# #' data frame (ID, ParentID, ChildreIDs, Node)
# tree2.build <- function(t, FUNC = print) {
#   dataframe <- t
#   children <- with(dataframe, aggregate(ID, by = list(ID = ParentID), paste, collapse = " "))
#   names(children) <- c("ID", "ChildrenIDs")
#   merge(dataframe, children, all.x = TRUE)
# }
# 
# tree2.parentID <- function(t, id) {
#   f <- filter(t, ID == id)
#   if(dim(f)[1] == 0) {
#     return (NULL)
#   }
#   f$ParentID
# }
# 
# tree2.childenIDs <- function(t, id) {
#   f <- filter(t, ID == id)
#   if (dim(f)[1] == 0)
#     return (NULL)
#   childrenIDs <- unlist(strsplit(f$ChildrenIDs, " ")) %>% setdiff(id)
#   result <- childrenIDs
#   for (childrenID in childrenIDs) {
#     resu
#     lt <- tree2.childenIDs(t, childrenID) %>% union(result)
#   }
#   result %>% setdiff(NA)
# }
# 
# new.tree.node <- function(name, data) {
#   
# }
# 
# delayedAssign("PARENTID", "ParentID")
