# CONTRIBUTIONS

This document outlines both our external contribution process and our internal team workflow.

Aurora is developed collaboratively by a three–member team. We follow structured Git practices to ensure code quality, transparency, and equal contribution.

---

## Team Contributions

Development responsibilities were distributed across the team as follows:

### Mohammad Zaed Iqbal Khan

- Initial development of core API wrapper  
- Orchestration of Aurora functions  
- Unit and integration test implementation  
- Error handling across primary functions  
- Git repository structure and file organization  

---

### Ilakiya Yuvarani Paulraj

- Merging of three API wrappers  
- Integration of wrappers into a unified main function  
- Inline code commenting  
- Documentation using Roxygen2  
- Vignette preparation  
- CRAN package setup and submission preparation  

---

### Karthik Chandran

- Development of revenue integration function  
- Development of correlation plotting function  
- Error handling for analytical functions  
- GitHub Actions CI/CD pipeline  
- README updates and maintenance  

---

## Workflow Order

The development process followed this sequential order:

1. Initial development of core API wrapper
2. Merging of three API wrappers
3. Integration of wrappers into a unified main function
4. Orchestration of Aurora functions
5. Error handling across primary functions
6. Git repository structure and file organization
7. Unit and integration test implementation
8. Inline code commenting
9. Documentation using Roxygen2
10. Vignette preparation
11. Development of revenue integration function
12. Development of correlation plotting function
13. Error handling for analytical functions
14. GitHub Actions CI/CD pipeline
15. README updates and maintenance
16. CRAN package setup and submission preparation  

---

## Team Workflow Practices

To ensure consistent collaboration and equal contribution, the team followed these workflow standards:

### Branching Strategy

- Each feature or fix was developed on its own branch.
- Branch naming followed:
  - `main` for main branch.
  - `Zaed` for development branch.
  - `Ilakiya` for development branch.
  - `Karthik` for development branch.
  - `Zaed_Two` for development branch.
  - `KC` for development branch.
  - `Testing` for testing branch. 


---

### Pull Requests

All changes were merged via Pull Requests:

- Every PR required at least one reviewer.
- CI checks (GitHub Actions) had to pass before merging.

Direct commits to `main` were avoided.

---

### Code Reviews

- Each team member reviewed others’ PRs.
- Feedback was addressed before merge.
- This ensured shared ownership of the codebase and knowledge transfer.

---

### Testing

- Unit and integration tests were added for new functionality.

### Documentation

- Functions are documented using Roxygen2.
- A vignette provides package usage examples.
- README is kept up to date with installation and usage instructions.

---

### Git History and Equal Contribution

The Git commit history demonstrates:

- All team members contributed code regularly.
- Feature branches and Pull Requests were consistently used.
- CI/CD enforcement through GitHub Actions.
- Balanced contribution across core development, testing, documentation, and deployment.

This history reflects adherence to the workflow described above and confirms equal participation by all group members.

---

### Reporting Issues

Please use GitHub Issues to report bugs or request features. Include:

- Clear description
- Steps to reproduce
- Expected vs actual behavior