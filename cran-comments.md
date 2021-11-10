# Version: 0.2.0

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results

-- visae 0.2.0: NOTE

  Build ID:   visae_0.2.0.tar.gz-a5e19a17ac98471a94af4cf4490d0030
  Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit

  Build time: 3m 53.6s

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Marcio A. Diniz <marcio.diniz@cshs.org>'

  Possibly misspelled words in DESCRIPTION:
    CTCAE (13:133)
    
-- visae 0.2.0: NOTE
  Build ID:   visae_0.2.0.tar.gz-94f1e5200c7a4e2596ff4f13cf78ad21
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Marcio A. Diniz <marcio.diniz@cshs.org>'

  Possibly misspelled words in DESCRIPTION:
    CTCAE (13:133)

-- visae 0.2.0: NOTE

  Build ID:   visae_0.2.0.tar.gz-a5bc0e31073947dcbcf3939cd926da95
  Platform:   Fedora Linux, R-devel, clang, gfortran

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Marcio A. Diniz <marcio.diniz@cshs.org>'

  Possibly misspelled words in DESCRIPTION:
    CTCAE (13:133)

# Version: 0.1.0

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Marcio Augusto Diniz <marcio.diniz@cshs.org>'
  
  New submission

## Previous Comments 

The Description field is intended to be a (one paragraph) description of what the package does and why it may be useful.
Please add more details about the package functionality and implemented methods in your Description text.

If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...) or if those are not available: <https:...> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

Please rather use the Authors@R field and declare Maintainer, Authors and Contributors with their appropriate roles with person() calls.
e.g. something like:
Authors@R: c(person("Alice", "Developer", role = c("aut", "cre","cph"),
                      email = "alice.developer@some.domain.net"),
               person("Bob", "Dev", role = "aut") )
