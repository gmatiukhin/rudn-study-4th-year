//! Наше приложение будет решать вымышленную задачу,
//! которую можно сформулировать при помощи следующих условий.
//!
//! Компания под названием XYZ производит два типа компьютеров:
//! «тип 1» и «тип 2». Тип компьютера определяется его архитектурой.
//!
//! Компьютеры фирмы XYZ могут выполнять ряд функций.
//! На данный момент определены четыре функции:
//! - сервер DDNS (DDNS Server),
//! - сервер DNS (DNS Server),
//! - шлюз (Gateway)
//! - и маршрутизатор (Router).
//!
//! Каждый компьютер перед выпуском проходит ряд тестов.
//! Тесты, выполняемые над каждым компьютером, зависят от его типа и выполняемой функции.
//! На данный момент определены пять тестов: «тест 1», «тест 2», «тест 3», «тест 4» и «тест 5».
//!
//! Для каждого проверяемого компьютера устанавливается предельный срок тестирования.
//! Все тесты, соответствующие данному компьютеру, должны быть выполнены не позднее указанной даты.
//! Сама дата зависит от тестов, выбранных для каждого конкретного компьютера.
//!
//! Большая часть процесса выполнения тестов в компании XYZ автоматизирована
//! при помощи внутреннего программного обеспечения,
//! которое выбирает конкретный набор тестов и определяет дату тестирования
//! на основе типа и функций компьютеров.
//!
//! Правила
//! На данный момент наборы тестов и даты их выполнения для конкретных типов компьютеров
//! выбираются в соответствии со следующими бизнес-правилами:
//! - Над компьютерами типа 1 должны быть выполнены тесты 1, 2 и 3.
//! - Над компьютерами типа 2, выполняющими функцию серверов DNS, должны быть выполнены тесты 4 и 5.
//! - Над компьютерами типа 2, выполняющими функцию серверов DDNS, должны быть выполнены тесты 2 и 3.
//! - Над компьютерами типа 2, выполняющими функцию шлюза, должны быть выполнены тесты 3 и 4.
//! - Над компьютерами типа 2, выполняющиими функцию маршрутизатора, должны быть выполнены тесты 1 и 3.
//! - Если среди тестов, выбранных для данного компьютера, есть тест 1,
//!   то тестирование должно производиться не позднее чем через три дня после даты производства.
//!   Данное правило является приоритетным по отношению ко всем последующим правилам
//!   выбора даты тестирования.
//! - Если среди тестов, выбранных для данного компьютера, есть тест 2,
//!   то тестирование должно производиться не позднее чем через семь дней после даты производства.
//!   Данное правило является приоритетным по отношению ко всем последующим правилам
//!   выбора даты тестирования.
//! - Если среди тестов, выбранных для данного компьютера, есть тест 3,
//!   то тестирование должно производиться не позднее чем через 10 дней после даты производства.
//!   Данное правило является приоритетным по отношению ко всем последующим правилам
//!   выбора даты тестирования.
//! - Если среди тестов, выбранных для данного компьютера, есть тест 4,
//!   то тестирование должно производиться не позднее чем через 12 дней после даты производства.
//!   Данное правило является приоритетным по отношению ко всем последующим правилам
//!   выбора даты тестирования.
//! - Если среди тестов, выбранных для данного компьютера, есть тест 5,
//!   то тестирование должно производиться не позднее чем через 14 дней после даты производства.
mod model;

use std::{
    cell::RefCell,
    path::{Path, PathBuf},
    rc::Rc,
    sync::Arc,
};

use model::*;
use serde_json::json;
use zen_engine::{
    handler::custom_node_adapter::NoopCustomNode,
    loader::{FilesystemLoader, FilesystemLoaderOptions},
    DecisionEngine,
};

use tokio::{
    fs::{self, DirEntry, File},
    io::AsyncReadExt,
};

use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the decision.
    /// Should correspond to the name of the file in ./decisions
    #[arg(short, long)]
    decision: String,

    /// Path to a directory with items to decide upon.
    #[arg(short, long)]
    items_dir: PathBuf,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    let engine = DecisionEngine::default().with_loader(Arc::new(FilesystemLoader::new(
        FilesystemLoaderOptions {
            root: "./decisions",
            keep_in_memory: true,
        },
    )));

    let deadline_decision = engine
        .get_decision(&format!("{}.json", args.decision))
        .await
        .unwrap();

    let pcs = get_pcs(&args.items_dir).await;
    for pc in pcs {
        println!("Evaluating: {:?}.", pc);
        let result = deadline_decision.evaluate(pc.into()).await;
        match result {
            Ok(resp) => println!("{:?}", resp),
            Err(e) => println!("{:?}", e),
        }
    }
}

async fn get_pcs(path: &Path) -> Vec<Pc> {
    async fn read_one_file(path: &Path) -> Pc {
        let mut file = File::open(&path)
            .await
            .unwrap_or_else(|_| panic!("Coult not open file: {:?}", &path));
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .await
            .unwrap_or_else(|_| panic!("Coult not read file: {:?}", &path));
        serde_json::from_str(&contents).unwrap_or_else(|_| panic!("Ill-formed JSON in: {:?}", path))
    }

    if path.is_dir() {
        let mut entries = fs::read_dir(&path)
            .await
            .unwrap_or_else(|_| panic!("Coult not read directory: {:?}", &path));

        let mut pcs = vec![];
        while let Some(entry) = entries.next_entry().await.unwrap() {
            if entry.metadata().await.unwrap().is_dir() {
                continue;
            }
            pcs.push(read_one_file(&entry.path()).await)
        }

        pcs
    } else {
        vec![read_one_file(path).await]
    }
}
