package wiki

case class Article(
  title: String,
  publishCategory: PublishCategory,
  mainCategory: String,
  extraCategories: List[String],
)
