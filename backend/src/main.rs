use actix_web::{web, App, HttpServer, web::Json, Result};
use serde::Serialize;

#[derive(Serialize)]
struct HelloJson {
    name: String
}

async fn send_hello() -> Result<Json<HelloJson>> {
    Ok(Json(HelloJson { name: "oppai".to_string() }))
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .route("/", web::get().to(send_hello))
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
