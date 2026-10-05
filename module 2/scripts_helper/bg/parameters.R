parameters = function() {

#Define here the capacity of each link, in bits/sec. The LinkCapacities matrix
#is a square matrix where the number of rows equals the number of nodes. Rows
#correspond to origin nodes and columns to destination nodes. Element(i,j)
#should be equal to the link capacity if there is a link from node i to node j,
#and should be 0 otherwise
LinkCapacities<<-matrix(c(0,  256e3, 256e3,   0,   0,   0,
                          0,    0,   0, 256e3, 256e3,   0,
                          0,    0,   0,   0,   256e3,   0,
                          0,    0,   0,   0,   0,   256e3,
                          0,    0,   0,   0,   0,   256e3,
                          0,    0,   0,   0,   0,       0),
                        nrow=6,
                        ncol=6,
                        byrow=TRUE)

#Define here the flows. Flows is a matrix with a user-defined number of rows and
#3 columns. Each row corresponds to a different flow, and the three columns
#correspond to (1) the origin node, (2) the destination node, and (3) the
#offered rate between the two previous nodes, expressed in bits/sec
Flows<<-matrix(c(1, 6, 343e3,
                 1, 5,  64e3,
                 2, 5, 128e3),
               nrow=3,
               ncol=3,
               byrow=TRUE)
}
