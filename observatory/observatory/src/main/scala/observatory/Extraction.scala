package observatory

import java.time.LocalDate

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types.{DataTypes, StructField, StructType}
import org.apache.log4j.{Level, Logger}
import org.apache.spark.rdd.RDD
/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)



  val sc: SparkSession = SparkSession.builder().appName("Observatory")
    .config("spark.executor.memory", "4G")
    .config("spark.master", "local").getOrCreate()

  val stnID = StructField("stnID", DataTypes.StringType)
  val wbanID = StructField("wbanID", DataTypes.StringType)
  val lat = StructField("latitude", DataTypes.DoubleType)
  val long = StructField("longitude", DataTypes.DoubleType)
  val month = StructField("month", DataTypes.IntegerType)
  val day = StructField("day", DataTypes.IntegerType)
  val temp = StructField("temperature", DataTypes.DoubleType)

  val statStruct = StructType(Array(stnID, wbanID, lat, long))
  val tempStruct = StructType(Array(stnID, wbanID, month, day, temp))


  import sc.implicits._

  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val statPath = getClass.getResource(stationsFile).getPath
    val tempPath = getClass.getResource(temperaturesFile).getPath

    val stats = sc.read.schema(statStruct).option("header", value = false).csv(statPath)
    val temps = sc.read.schema(tempStruct).option("header", value = false).csv(tempPath)

    val filteredStations = stats.filter("latitude IS NOT NULL and longitude IS NOT NULL")
    val joined = filteredStations.join(temps, stats("stnID") <=> temps("stnID") &&  stats("wbanID") <=> temps("wbanID"))

    joined.rdd.map(row => {
      val temperature: Double = (row.getAs[Double]("temperature") - 32) * 5 / 9
      val location: Location = Location(row.getAs[Double]("latitude"), row.getAs[Double]("longitude"))
      val localDate: LocalDate = LocalDate.of(year, row.getAs[Int]("month"), row.getAs[Int]("day"))
      (localDate, location, temperature)
    }).collect


  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val recordsRDD: RDD[(Location, Temperature)] = sc.sparkContext.parallelize(records.toSeq).map { case (_, location, temp) => (location, temp) }
    val recordsDS = recordsRDD.toDS()
      .withColumnRenamed("_1", "location")
      .withColumnRenamed("_2", "temperature")

    recordsDS.groupBy("location").mean("temperature").as[(Location, Double)].collect
  }

}
