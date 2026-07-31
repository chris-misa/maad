# Overview of MAAD ideas


**What is MAAD?**

MAAD (Multifractal Address-structure Anomaly Detection) is a new way to detect anomalous IP addresses that may be associated with malicious or unsolicited network traffic.
It is based on the intuition that under normal conditions sets of observed IP addresses (e.g., the set of all source IPv4 addresses ingressing an edge network over a 15-minute time window) have distinctive *spatial* structures.
By spatial structure, we mean how addresses are arranged relative to each other in the IP address spaces (e.g., IPv4 or IPv6).


**How does MAAD work?**

MAAD works by looking at a set of IP addresses and computing metrics (derived from multifractal analysis) that summarize the relative prefix-level relationships between them.
There are fine-grained metrics that summarize how an individual IP address "fits into" the whole set under consideration.
There are also coarse-grained metrics that summarize the distinct structure of the whole set of addresses.
Which metrics you use, depends on your usecase---what particular types of anomalies are you trying to capture.
In either case, the intuition is that when these metrics change, the spatial structure of the addresses under consideration is changing.


**Detecting single-address anomalies**



**Detecting anomalous sets of addresses**



