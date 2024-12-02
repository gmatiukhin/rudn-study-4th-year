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

use std::sync::Arc;

use serde_json::json;
use zen_engine::{
    handler::custom_node_adapter::NoopCustomNode,
    loader::{FilesystemLoader, FilesystemLoaderOptions},
    DecisionEngine,
};

mod model;

fn main() {
    let engine = DecisionEngine::new(
        Arc::new(FilesystemLoader::new(FilesystemLoaderOptions {
            root: "/decisions",
            keep_in_memory: true,
        })),
        Arc::new(NoopCustomNode),
    );
}