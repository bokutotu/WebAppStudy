use actix_web::{web, App, HttpServer, HttpResponse};

// struct HelloJson {
//     content: String
// }

async fn send_hello() -> HttpResponse {
    // let hellow_world = HelloJson {
    //     content: "Hello World".to_string()
    // };
    HttpResponse::Ok().body("hello world")
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
