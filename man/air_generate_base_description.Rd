% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/air_dump.R
\name{air_generate_base_description}
\alias{air_generate_base_description}
\title{Generate descriptive metadata}
\usage{
air_generate_base_description(
  title = NA,
  creator = NA,
  created = NA,
  primary_contact = NA,
  email = NA,
  description = NA,
  contributor = NA,
  identifier = NA,
  license = NA,
  ...
)
}
\arguments{
\item{title}{String. Title is a property that refers to the name or names by
which a resource is formally known.}

\item{creator}{String. Person or people who created the base}

\item{created}{String. When was the base created?}

\item{primary_contact}{String.  Person or entity primarily responsible for
making the content of a resource}

\item{email}{String. Email of primary_contact}

\item{description}{String. This property refers to the description of
the content of a resource. The description is a potentially rich source of
indexable terms and assist the users in their selection of an appropriate
resource.}

\item{contributor}{String. An entity responsible for making contributions to the resource.}

\item{identifier}{String. An unambiguous reference to the resource within a given context.}

\item{license}{String. A legal document giving official permission to do something with the resource. "CC BY 4.0"}

\item{...}{String. Additional descriptive metadata elements. See details.
Additional elements can be added as name pair values e.g.
\code{ isPartOf = "https://doi.org/00.00000/MyPaper01", isReferencedBy = "https://doi.org/10.48321/MyDMP01"}}
}
\value{
data.frame with descriptive metadata
}
\description{
Creates a data.frame that describes the base. Descriptive metadata provides
information about the base as a whole: who created it,
why, when, where can data be accessed, keywords, what license governs data use, etc.
Descriptive metadata facilitates data reuse by providing a point of contact for
future users, as well as attributes that allow the data to be entered into searchable
catalogs or archives.
}
\details{
See  \href{https://www.dublincore.org/resources/userguide/creating_metadata/}{dublin core} for inspiration about additional attributes.
}
\examples{

air_generate_base_description(title = "My Awesome Base" ,
 primary_contact= "Base Creator/Maintainer",
 email = "email@example.com",
 base_description = "This base contains my awesome data
 from a project studying XXX in YYY. Data in the base were collected
 from 1900-01-01 to 1990-01-01 by researchers at Some Long Term Project.",
 is_part_of = "https://doi.org/10.48321/MyDMP01",
 isReferencedBy = "https://doi.org/10.5072/zenodo_sandbox.1062705"
 )

}
