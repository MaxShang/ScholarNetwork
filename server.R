library(shiny)
shinyServer(function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    if(!require("devtools")) install.packages("devtools")
    library("devtools")
    install_github("pablobarbera/scholarnetwork",force=FALSE)
    library("scholar")
    
    extractAuthors <- function(x){
      authors <- unlist(stringr::str_split(x, ","))
      # deleting empty authors
      authors <- authors[grepl('[A-Za-z]+', authors)]
      # cleaning author list
      authors <- stringr::str_trim(authors)
      # keeping only initial of first name
      first <- gsub('(^[A-Z]{1}).*', authors, repl="\\1")
      last <- gsub("^[A-Z]* ([[:alnum:]'’]+).*", authors, repl="\\1")
      #fix curly apostrophes
      last <- gsub('’', "\\'", last)
      # fixing capitalization of last name
      last <- gsub("(^|'|’|[[:space:]])([[:alpha:]])", "\\1\\U\\2", last, perl=TRUE)
      last <- stringr::str_to_title(last)
      authors <- paste(first, last, sep=" ")
      # if more than one author, create edge list
      if (length(authors)>1){
        edges <- as.data.frame(t(combn(x=authors, m=2)), stringsAsFactors=F)
        names(edges) <- c("node1", "node2")
        edges$weight <- 1/length(authors)
        return(edges)
      }
      if (length(authors)<=1) return(NULL)
    }
    extractNetwork <- function(id, n=500, largest_component=FALSE, ...){
      
      # downloading publications
      pubs <- scholar::get_publications(id=id, pagesize=n, ...)
      
      # converting to edges
      edges <- lapply(pubs$author, extractAuthors)
      edges <- do.call(rbind, edges)
      edges <- aggregate(edges$weight,
                         by=list(node1=edges$node1, node2=edges$node2),
                         FUN=function(x) sum(x))
      names(edges)[3] <- "weight"
      
      # extracting node-level information
      network <- igraph::graph.edgelist(as.matrix(edges[,c("node1", "node2")]), 
                                        directed=FALSE)
      igraph::edge_attr(network, "weight") <- edges$weight
      
      ### SELECT LARGEST COMPONENT
      if (largest_component==TRUE){
        network <- decompose(network)[[1]]
      }
      
      fc <- igraph::walktrap.community(network)
      nodes <- data.frame(label = igraph::V(network)$name,
                          degree=igraph::strength(network), group=fc$membership,
                          stringsAsFactors=F)
      nodes <- nodes[order(nodes$label),]
      if (largest_component==TRUE){
        edges <- edges[edges$node1 %in% nodes$label & edges$node2 %in% nodes$label,]
      }
      return(list(nodes=nodes, edges=edges))
    }
    
    citid <- strsplit((strsplit(input$scholarID,"&",fixed = TRUE)[[1]][1]),"=",fixed = TRUE)[[1]][2]
    d <- extractNetwork(id=citid, n=500)
    library(ggplot2)
    library(igraph)
    # cleaning network data
    network <- graph_from_data_frame(d$edges, directed=FALSE)
    set.seed(123)
    l <- layout.fruchterman.reingold(network, niter=1500) # layout
    fc <- walktrap.community(network) # community detection
    
    # node locations
    nodes <- data.frame(l); names(nodes) <- c("x", "y")
    nodes$cluster <- factor(fc$membership)
    nodes$label <- fc$names
    nodes$degree <- degree(network)
    
    # edge locations
    edgelist <- get.edgelist(network, names=FALSE)
    edges <- data.frame(nodes[edgelist[,1],c("x", "y")], nodes[edgelist[,2],c("x", "y")])
    names(edges) <- c("x1", "y1", "x2", "y2")
    
    # and now visualizing it...
    p <- ggplot(nodes, aes(x=x, y=y, color=cluster, label=label, size=degree))
    pq <- p  +
      # nodes
      geom_point(color="white", aes(fill=cluster,size=degree*8),
                 shape=21, alpha=1/3.5,stroke=15) +
      
      # geom_point(color="black", size=4.5)+
      # geom_point(color="pink", size=4)+
      # geom_point(aes(shape = cluster))+
      
      # edges
      geom_curve(
        aes(x=x1, y=y1, xend=x2, yend=y2,label=NA),
        data=edges, size=0.25, color="gray85", alpha=1/1.8,curvature = 0.09) +
      ## note that here I add a border to the points
      scale_fill_discrete(labels=labels) +
      scale_size_continuous(range = c(4, 25)) +
      geom_text(color="gray31", aes(size=degree))+
      #theme_economist() + 
      #scale_color_economist()
      theme(
        panel.background = element_rect(fill = "gray75"),
        plot.background = element_rect(fill="gray100"),
        axis.line = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
        # ,
        #
        # legend.background = element_rect(colour = F, fill = "black"),
        # legend.key = element_rect(fill = "darkgrey", colour = F),
        # legend.title = element_text(color="white"),
        # legend.text = element_text(color="white")
      ) #+
      ## changing size of points in legend
      #guides(fill = guide_legend(override.aes = list(size=5)))
    
    pq

  })

})
