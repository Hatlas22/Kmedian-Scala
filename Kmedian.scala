import scala.util.Random

case class Point(x: List[Int]){
    def hd( b: List[Int]) = { 
    var c = Array[Int]()
    var i = 0
    while(i < this.x.length) {  
        c = c :+ (this.x(i)^b(i))
        i+=1 }
    c.sum
    }
}

case class Cluster(centroid: Point, points: List[Point] = Nil)

case class ClusterBatch(clusters: List[Cluster])


def Kmedian(allPoints: List[Point], n: Int, k: Int): List[ClusterBatch]= {
    var innitCluster = initialClusters(allPoints, n)
    var initialBatch = ClusterBatch(innitCluster)
    var assignedClusters = List[Cluster]()
    var reCentered : ClusterBatch = null
    var i = 0
    while(i < k){
        assignedClusters = assignement(allPoints, initialBatch.clusters)
        reCentered = ClusterBatch(assignedClusters)
        i = i+1    
    }
    List(reCentered)
}

def assignement(allPoints: List[Point], clusters: List[Cluster]): List[Cluster] = {
    val ptsByCluster = allPoints.groupBy(p => clusters.minBy(_.centroid.hd(p.x)))
    clusters.map(c =>
        c.copy(points = ptsByCluster.getOrElse(c, Nil))
    )
}

def initialClusters(allPts: List[Point], n: Int): List[Cluster] = 
    Random.shuffle(allPts).take(n).map(pt => Cluster(pt))

def reCenter(clusters: List[Cluster]): List[Cluster] = clusters.map { c =>
    var sumVect = List[Int]()
    var newCentroid = List[Int]()
    var barycenter = List[Int]()
    sumVect = c.points.map(_.x).transpose.map(_.sum)
    barycenter = sumVect.map( j => if(j >= (c.points.size+1)/2 ){1}else{0})
    c.copy(centroid = Point(barycenter))
}
