package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.mamma.enums.MammaDense2Studiegroep;
import nl.rivm.screenit.model.project.GroepInvoer;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.repository.algemeen.ProjectGroepRepository;
import nl.rivm.screenit.repository.algemeen.ProjectRepository;
import nl.rivm.screenit.repository.mamma.MammaDossierRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.impl.BaseProjectBestandVerwerkingContext;

import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;

@Slf4j
public class MammaDense2ImportVerwerkingContext extends BaseProjectBestandVerwerkingContext
{

	private static final String PSEUDO_ID = "pseudoid";

	private static final String STUDIEGROEP = "studiegroep";

	public static final List<String> STUDIE_PROJECT_ATTRIBUTEN = List.of("meldcode", "studieid", "behandelcentrum");

	private final Map<MammaDense2Studiegroep, Project> projecten = new HashMap<>();

	private final Map<MammaDense2Studiegroep, ProjectGroep> projectGroepen = new HashMap<>();

	private final String bestandsNaam;

	private int pseudoIdColumn = -1;

	private int studiegroepColumn = -1;

	private int geslaagd = 0;

	private int mislukt = 0;

	private boolean fout = false;

	@Getter
	private String meldingen = "";

	public MammaDense2ImportVerwerkingContext(File file)
	{
		super(file);
		this.bestandsNaam = file.getName();
	}

	protected void bepaalHeaderPosities()
	{
		var headers = getHuidigeRegel();
		for (var studiegroep : MammaDense2Studiegroep.values())
		{
			var project = findProject(studiegroep);
			bepaalHeaderPosities(project, headers);
		}
	}

	@Override
	public Collection<ProjectAttribuut> getAttributen()
	{
		return getAttributenPositie().keySet().stream().filter(attribuut -> attribuut.getProject().equals(getProject())).collect(Collectors.toList());
	}

	@Override
	protected void checkClientIdentifingHeaders()
	{
		if (pseudoIdColumn == -1 || studiegroepColumn == -1)
		{
			throw new IllegalStateException("Geen header 'PseudoID' en/of 'Studiegroep' gevonden in het bestand.");
		}
		if (getAttributenPositie().keySet().stream().map(a -> a.getNaam().toUpperCase()).distinct().collect(Collectors.toList()).size() != 3)
		{
			throw new IllegalStateException("Er staan niet precies 3 attributen (naast PseudoID en Studiegroep) in het bestand.");
		}
	}

	@Override
	public Project getProject()
	{
		var studiegroep = getStudiegroep();
		return projecten.computeIfAbsent(studiegroep, this::findProject);
	}

	@Override
	public ProjectBestandType getType()
	{
		return ProjectBestandType.POPULATIE;
	}

	@Override
	public ProjectGroep getGroep()
	{
		var studiegroep = getStudiegroep();
		return projectGroepen.computeIfAbsent(studiegroep, k -> maakProjectGroep());
	}

	@Override
	public boolean heeftAttributen()
	{
		return true;
	}

	@Override
	public void verwerkingGeslaagd()
	{
		geslaagd++;
	}

	@Override
	public void verwerkingMislukt(IllegalStateException e)
	{
		addBestandsMelding(getHuidigeRegelnummer(), e.getMessage());
		mislukt++;
		LOG.warn("Verwerken regel {} in bestand {} mislukt. {}", getHuidigeRegelnummer(), bestandsNaam, e.getMessage());
	}

	@Override
	public Client getClient()
	{
		var dossierID = getHuidigeRegel().get(pseudoIdColumn);
		try
		{
			var dossier = getBean(MammaDossierRepository.class).findById(Long.parseLong(dossierID))
				.orElseThrow(() -> new IllegalStateException("Geen client gevonden met PseudoId: " + dossierID));
			return dossier.getClient();
		}
		catch (NumberFormatException e)
		{
			throw new IllegalStateException("PseudoId is geen getal: " + dossierID);
		}
	}

	@Override
	protected boolean addClientIdentifingHeader(String header)
	{
		var gevonden = false;
		var headers = getHuidigeRegel();
		var geformateerdeHeader = header.toLowerCase().trim();
		if (PSEUDO_ID.equals(geformateerdeHeader))
		{
			pseudoIdColumn = headers.indexOf(header);
			gevonden = true;
		}
		else if (STUDIEGROEP.equals(geformateerdeHeader))
		{
			studiegroepColumn = headers.indexOf(header);
			gevonden = true;
		}
		return gevonden;
	}

	@Override
	protected void headerNietGevonden(Project project, String geformateerdeHeader)
	{
		var cgProjectnaam = getProjectNaam(MammaDense2Studiegroep.CG);
		if (!project.getNaam().equals(cgProjectnaam))
		{
			super.headerNietGevonden(project, geformateerdeHeader);
		}
	}

	@Override
	protected void addBestandsMelding(Integer regelnummer, String melding)
	{
		fout = true;
		meldingen += (regelnummer != null ? "Regelnummer: " + regelnummer + " " : "") + "Melding: " + melding + "<br>";
	}

	@Override
	public String getAttribuutWaarde(ProjectAttribuut attribuut)
	{
		String waarde;
		if (getStudiegroep() != MammaDense2Studiegroep.CG)
		{
			waarde = super.getAttribuutWaarde(attribuut);
		}
		else
		{
			waarde = getHuidigeRegel().get(getAttributenPositie().get(attribuut));
		}
		if (StringUtils.defaultIfBlank(waarde, "").length() > 255)
		{
			throw new IllegalStateException("Attribuut mag niet langer dan 255 tekens bevatten: " + attribuut.getNaam());
		}
		return waarde;
	}

	public boolean heeftMislukt()
	{
		return mislukt > 0 || fout;
	}

	public String getRapportage()
	{
		return "Geslaagd: " + geslaagd + " Mislukt: " + mislukt;
	}

	private Project findProject(MammaDense2Studiegroep studiegroep)
	{
		var projectNaam = getProjectNaam(studiegroep);
		var project = getBean(ProjectRepository.class).findOneByNaam(projectNaam)
			.orElseThrow(() -> new IllegalStateException("Project voor studiegroep '" + studiegroep + "' niet gevonden."));

		if (studiegroep != MammaDense2Studiegroep.CG)
		{
			var verplichteAttributen = STUDIE_PROJECT_ATTRIBUTEN;
			var gevondenAantalVerplichteAttributen = project.getProjectAttributen().stream().filter(attribuut -> verplichteAttributen.contains(attribuut.getNaam().toLowerCase()))
				.collect(Collectors.toSet()).size();
			if (gevondenAantalVerplichteAttributen != verplichteAttributen.size())
			{
				throw new IllegalStateException("Project voor studiegroep '" + studiegroep + "' heeft niet precies 3 verplichte attributen.");
			}
		}
		return project;
	}

	private ProjectGroep maakProjectGroep()
	{
		var project = getProject();
		var projectGroepOptional = getBean(ProjectGroepRepository.class).findFirstByNaamAndProject(bestandsNaam, project);
		if (projectGroepOptional.isPresent())
		{
			return projectGroepOptional.get();
		}
		var projectGroep = new ProjectGroep();
		projectGroep.setNaam(bestandsNaam);
		projectGroep.setProject(project);
		project.getGroepen().add(projectGroep);
		projectGroep.setActief(true);
		projectGroep.setGroepInvoer(GroepInvoer.CRITERIA);
		projectGroep.setActiefDatum(getBean(ICurrentDateSupplier.class).getDate());
		projectGroep.setPopulatie(0);
		return getBean(ProjectGroepRepository.class).save(projectGroep);
	}

	private @NotNull MammaDense2Studiegroep getStudiegroep()
	{
		try
		{
			return MammaDense2Studiegroep.valueOf(getStudiegroepString());
		}
		catch (IllegalArgumentException e)
		{
			throw new IllegalStateException("Studiegroep '" + getStudiegroepString() + "' is niet bekend in ScreenIT.");
		}
	}

	private @NotNull String getStudiegroepString()
	{
		return StringUtils.defaultIfBlank(getHuidigeRegel().get(studiegroepColumn), "").toUpperCase().trim();
	}

	private static String getProjectNaam(MammaDense2Studiegroep studiegroep)
	{
		var projectNaam = (String) getBean(OrganisatieParameterService.class).getOrganisatieParameter(null, studiegroep.getProjectParameter());
		if (StringUtils.isBlank(projectNaam))
		{
			throw new IllegalStateException("Projectnaam voor studiegroep '" + studiegroep + "' niet gevonden.");
		}
		return projectNaam;
	}
}
