# Conflict solving and cooperated planning in multi-agent systems

## Abstract

Multi-agent systems (MAS) are integral to the advancement of artificial intelligence,
enabling autonomous agents to collaborate and resolve conflicts in dynamic environments.
This scoping review protocol, guided by the PRISMA-ScR framework,
aims to explore algorithms for cooperative planning and conflict resolution in MAS.
The review seeks to identify existing approaches, highlight challenges, evaluate effectiveness,
and uncover research gaps. By synthesizing the current body of knowledge,
this study will inform future developments in MAS design,
focusing on scalable, autonomous, and efficient decision-making systems.

## Introduction

Multi-agent systems (MAS) represent a critical area in artificial intelligence,
focusing on how independent agents interact to achieve common or conflicting goals.
As these systems grow in complexity,
enabling efficient cooperation and conflict resolution becomes essential.

The main objective is to map existing approaches, identify key challenges,
and highlight unresolved research questions.
Through systematic identification and evaluation of relevant literature,
this review aims to provide a comprehensive understanding of how MAS are conceptualized,
operationalized, and evaluated across different contexts.

### Rationale

The increasing complexity of MAS necessitates advanced approaches
to ensure effective collaboration and conflict resolution among agents.
As MAS are deployed in diverse environments from autonomous vehicles to robotic swarms
the ability to plan cooperatively while resolving conflicts is critical for system efficiency and reliability.
Understanding existing methodologies and identifying research gaps is essential for advancing the field.

### Objectives

The primary objectives of this scoping review are:

RQ#1. Highlight common approaches to domain modeling in MAS planning.  
RQ#2. Identify strategies for conflict resoulution between agents in MAS.  
RQ#3. Determine the mechanisms that enable cooperation in MAS.  
RQ#4. Assess the effectiveness of different frameworks and strategies.

### Scope of the Review

This review follows the PRISMA-ScR framework to explore algorithms used in conflict-solving and cooperative planning in MAS. Through systematic identification and evaluation of relevant literature, this review aims to provide a comprehensive understanding of how MAS are conceptualized, operationalized, and evaluated across different contexts.

## Protocol

### Design

The review will be conducted in accordance with the framework
and principles outlined by Arksey and O’Malley and the refinements proposed by Levac et al.
Additionally, we will utilize updated guidance developed by the Joanna Briggs Institute (JBI).
The Preferred Reporting Items for Systematic Reviews and Meta-Analyses
extension for Scoping Reviews (PRISMA-ScR) guidelines will be adhered to throughout the process.
There will be five stages in the review process:

1. Identifying the Research Questions:
   We will formulate specific research questions to guide the scoping review,
   focusing on how artificial intelligence impacts learning outcomes
   for school and university students.
2. Identifying Relevant Studies:
   A comprehensive search strategy will be developed to identify studies
   that explore the intersection of artificial intelligence and education,
   specifically targeting relevant academic databases such as ScienceDirect,
   Google Scholar and Colab.
3. Study Selection:
   Inclusion and exclusion criteria will be established to select studies
   based on their relevance to our research questions and the quality of evidence they provide.
4. Data Extraction/Charting the Data:
   Data will be systematically extracted from the selected studies,
   focusing on key elements such as study design, population, context,
   and outcomes related to the use of artificial intelligence in educational settings.
5. Collating, Summarizing, and Reporting Results:
   The extracted data will be collated and summarized to identify key themes
   and trends regarding the impact of artificial intelligence on learning outcomes.
   A narrative synthesis will be conducted to report the findings.

#### Stage 1: Identifying the Research Questions

A preliminary search of relevant literature was undertaken in one database (ScienceDirect)
using "multi-agent systems AND (cooperated planning OR conflict solving)" query
to generate an initial understanding of how .
Our main goal is to understand the problems that researchers and engineers face
when designing multi-agent automated systems.

The following research questions were identified through this iterative process:

1. How is the real world problem transformed into a model for MAS?
1. What algorithms are being proposed to make the agents cooperate?
1. Are the algorithms domain-dependent or can they be used for any MAS?
1. How are the alrorithms evaluated?
1. What are the benefits of the proposed algorithms?

Through seeking to answer the above questions we will determine
gaps in the literature which need to be addressed
as part of future research on multi-agent autonomous systems.

#### Stage 2: Identifying Relevant Studies

The "PCC" mnemonic (population, concept, and context) is
recommended by the Joanna Briggs Institute to construct clear
inclusion criteria for scoping review and identify the focus and context of the review.

##### Population

Systems with multiple autonomous agents.

##### Concept

Algorithms used in order to enable cooperation
and conflict-resolution between multiple autonomous agents.

##### Context

This review will include studies focused on the methods for cooperation in
systems with multiple autonomous agents. For the purposes of this review
we use a broad definition of an "autonomous agent".
An "autonomous agent" is an intelligent entity capable of percieveing its
environmnet, making decisions, and taking actions independently
to achieve specific goals. Its autonomy includes bounded rationality,
allowing for efficient decision-making within computational
and communication constraints. Crucially, this definition does not limit
an "autonomous agent" to being a robot, unmanned surface vehicle or aircraft.

Both qualitative and quantitative primary research studies will be eligible.
Editorials, commentaries, and opinion pieces as well as conference abstracts will be excluded.

| Criterion         | Inclusion                                                                                                                                                                 | Exclusion                                                                            | Justification                                                                                                                                 |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------- |
| Population        | Systems with multiple autonomous agents. This includes systems in both real environments (robotic swarm, delivery robots) as well as in virtual or simulated ones.        | Systems with one one agent.                                                          | The focus of the review is conflict solving and cooperation which is to model in systems with only one agent.                                 |
| Concept           | Algorithms that are used to enable cooperation between autonomous agents in MAS.                                                                                          | Algorithms that focus on single-agent systems.                                       | The review is focused on algorithms that facilitate cooperation in MAS.                                                                       |
| Context           | Studies which focuse on developing algorithms to enable cooperated planning and conflict solving in systems with multiple autonomous agents with artificial intelligence. | Studies focused on conflict solving outside of the field of artificial intelligence. | The review is focused on the algorithms used in the field artificial intelligence, therefor all other fields are irrelevant for its purposes. |
| Language          | English, Russian, German                                                                                                                                                  | Other                                                                                | Reviewers speak only English, Russian and German and do not have the resources to translate articles from other languages.                    |
| Types of articles | Peer-reviewed scientific articles, empirical research                                                                                                                     | Non-peer-reviewed articles, commend, letters to the editor.                          | The use of peer-reviewed sources ensures the reliability and scientific validity of the data presented in the review.                         |
| Geographic        | Any                                                                                                                                                                       | None                                                                                 | Research of conflict solving in multi-agent systems does not depend on the country of origin.                                                 |
| Time period       | Any                                                                                                                                                                       | None                                                                                 | There is not a lot of research in this field, so the decision was made to focuse on all the literature available.                             |

#### Search Strategy

| Publication details    | Associated question(s)                                  |
| ---------------------- | ------------------------------------------------------- |
| Study title            | -                                                       |
| Author(s)              | Who are the authors of the study/document?              |
| Year of publication    | What year was the study/document published?             |
| Publication type       | Is the document an empirical study?                     |
| Origin                 | What is the country of origin of the study/document?    |
| General details        |
| ---------------------- |
| Aims/purpose           | What are the study/document aims?                       |
| Study design           | What is the study/document design                       |
| Study setting          | What real-life or imaginary system the study describes? |
| Study population       | What is the described multi-agent system?               |
| Content                |
| ---------------------- |
| Methodology details    | What is the problem domain?                             |
|                        | How is the problem domain modeled?                      |
|                        | Algorithm description                                   |
|                        | Is the algorithm domain-specific?                       |
| Measuring the outcomes | How is "success" defined?                               |
|                        | How is the algorhitm evaluated?                         |
|                        | What are the benefits of the algorithm?                 |

#### Stage 3: Study Selection

The titles and abstracts of the retrieved articles will be screened by one reviewer.
If there is any uncertainty about whether to include an acricle
based on the initial screning, it will also be included for full-text evaluation.
Articles that are identified as potentially meeting the inclusion/exclusion criteria
will be advanced to the full-text review.
Similar to the title and abstract screening the articles will be assessed by one reviewer.
The final results of the search, including the number of articles screened,
included and excluded at each stage will be illustrated in a PRISMA flow diagram,
as recommended by the PRISMA-ScR guidelines. This will visually represent
the study selection process and ensure adherence to reporting standards.

#### Stage 4: Data Extraction

Data will be extracted by one member of the research team into Microsoft Excel software
following guidelines from JBI.
Data to be extracted will include study characteristics
and characteristics of multi-agent autonomous systems related to our research questions.

#### Stage 5: Collating, Summarizing, and Reporting Results

##### Step 1: Collating and Summarising the Results

The initial analysis will involce both quantitative and qualitative approaches.
A descriptive numerical summary of the studies will be conducted,
covering aspects such as the number of studies, publication years, study populations,
and key methodologies used.
A deductive content analysis will be performed based on the research questions,
and a narrative summary will accompany the tabular results.
To explore research gaps, an inductive thematic analysis,
following the guidance of Braun and Clarke, will be conducted.

##### Step 2: Reporting

Results will be reported in line with the PRISMA-ScR guidelines.
A PRISMA flow diagram will illustrate the study selection process,
including reasons for exclusion during the full-text review.
Quantitative results will be presented in tables and organized in a narrative summary
aligned with the research questions.
The results will aim to achieve the following objectives:

1. Provide a clearer understanding of the methodology for developing autonomous multi-agent systems,
   capable of cooperation and conflict solving between their members;
1. Explore the key practical challenges in designing such systems;
1. Highlight the gaps in evidence.

##### Step 3: Implications

The review fundings will hel develop an understanding of how multi-agent autonomous systems
are conceptualised, operationalised and evaluated within the literature.
The broader implications of the findings for practice and research will be highlighted.

## Conclusion

This scoping review protocol establishes
a structured approach to investigating conflict resolution
and cooperative planning in multi-agent systems.
By adhering to the PRISMA-ScR framework, the review will systematically
examine how current methodologies address collaboration and conflict-solving challenges.

The findings will highlight research trends, practical challenges, and critical gaps.
The results are expected to inform future research and guide the development
of more robust and scalable MAS capable of efficient,
autonomous decision-making in complex environments.
