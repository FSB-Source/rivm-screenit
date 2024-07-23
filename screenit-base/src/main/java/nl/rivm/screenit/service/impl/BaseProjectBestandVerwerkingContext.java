package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;
import nl.topicuszorg.util.collections.CollectionUtils;

import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;

import au.com.bytecode.opencsv.CSVReader;
import ca.uhn.hl7v2.util.StringUtil;

@Slf4j
public abstract class BaseProjectBestandVerwerkingContext implements AutoCloseable
{
	private CSVReader reader = null;

	@Getter(AccessLevel.PROTECTED)
	private final Map<ProjectAttribuut, Integer> attributenPositie = new HashMap<>();

	private String[] huidigeRegel = null;

	@Getter(AccessLevel.PROTECTED)
	private int huidigeRegelnummer;

	private final File file;

	public BaseProjectBestandVerwerkingContext(File file)
	{
		this.file = file;
	}

	public void init() throws Exception
	{
		getMostUsefullReader(file);
		bepaalHeaderPosities();
		huidigeRegelnummer = 1;
	}

	private void getMostUsefullReader(File file) throws Exception
	{
		CSVReader testReader = null;
		try
		{
			testReader = new CSVReader(new FileReader(file), ',');
			huidigeRegel = testReader.readNext();
			if (huidigeRegel != null && Arrays.asList(huidigeRegel).size() >= 2)
			{
				reader = testReader;
			}
			else
			{
				testReader.close();
				huidigeRegel = null;
				testReader = new CSVReader(new FileReader(file), ';');
				huidigeRegel = testReader.readNext();
				if (huidigeRegel != null && Arrays.asList(huidigeRegel).size() >= 2)
				{
					reader = testReader;
				}
				else
				{
					testReader.close();
					testReader = null;
					throw new IllegalStateException("Bestand dat is aangeleverd voldoet niet aan het juiste formaat");
				}
			}
		}
		catch (Exception e)
		{
			if (testReader != null)
			{
				try
				{

					testReader.close();
				}
				catch (IOException e1)
				{
					LOG.error("Van het projectbestand kan de reader niet worden gesloten", e);
					throw e;
				}
			}
		}
	}

	protected void bepaalHeaderPosities()
	{
		var headers = getHuidigeRegel();
		bepaalHeaderPosities(getProject(), headers);
	}

	protected void bepaalHeaderPosities(Project project, List<String> headers)
	{
		var attributen = project.getProjectAttributen();
		var attributenInBestand = false;
		if (CollectionUtils.isNotEmpty(headers))
		{
			for (var header : headers)
			{
				var gevonden = addClientIdentifingHeader(header);
				var geformateerdeHeader = header.toLowerCase().trim();
				if (!gevonden)
				{
					for (var attribuut : attributen)
					{
						if (attribuut.getActief() && attribuut.getNaam().toLowerCase().equals(geformateerdeHeader))
						{
							attributenPositie.put(attribuut, headers.indexOf(header));
							addAttribuutHeaderHook();
							attributenInBestand = true;
							gevonden = true;
							break;
						}
					}
				}
				if (!gevonden)
				{
					headerNietGevonden(project, geformateerdeHeader);
				}
			}
			checkClientIdentifingHeaders();
		}
		if (CollectionUtils.isEmpty(attributen) && attributenInBestand)
		{
			throw new IllegalStateException(project.getNaam() + " project heeft (nog) geen attributen");
		}
	}

	protected void headerNietGevonden(Project project, String geformateerdeHeader)
	{
		addBestandsMelding(null, "Er is geen attribuut gevonden met naam " + geformateerdeHeader);
	}

	public List<String> getHuidigeRegel()
	{
		return Arrays.asList(huidigeRegel);
	}

	public boolean volgendeRegel() throws IOException
	{
		huidigeRegel = reader.readNext();
		if (huidigeRegel != null)
		{
			huidigeRegelnummer++;
			return true;
		}
		return false;
	}

	public String getAttributenGevondenString()
	{
		var attributenGevonden = getAttributen().stream().map(ProjectAttribuut::getNaam).collect(Collectors.joining(", "));
		return StringUtils.truncate(attributenGevonden, 250);
	}

	public String getAttribuutWaarde(ProjectAttribuut attribuut)
	{
		var waarde = getHuidigeRegel().get(getAttributenPositie().get(attribuut));
		if (StringUtil.isBlank(waarde))
		{
			throw new IllegalStateException("Geen waarde gevonden van attribuut " + attribuut.getNaam() + ". Client is wel toegevoegd aan project(groep).");
		}
		return waarde;
	}

	public void close() throws IOException
	{
		if (reader != null)
		{
			reader.close();
		}
	}

	protected static @NotNull <T> T getBean(Class<T> requiredType)
	{
		return ApplicationContextProvider.getApplicationContext().getBean(requiredType);
	}

	protected abstract void checkClientIdentifingHeaders();

	protected abstract boolean addClientIdentifingHeader(String header);

	protected void addAttribuutHeaderHook()
	{

	}

	protected abstract void addBestandsMelding(Integer regelnummer, String melding);

	public abstract Client getClient();

	public abstract Project getProject();

	public abstract void verwerkingGeslaagd();

	public abstract ProjectBestandType getType();

	public abstract ProjectGroep getGroep();

	public abstract Collection<ProjectAttribuut> getAttributen();

	public abstract boolean heeftAttributen();

	public abstract void verwerkingMislukt(IllegalStateException e);

	public void attribuutGewijzigd()
	{

	}

	public void clientGeinactiveerd()
	{
	}

	public void clientGeheractiveerd()
	{
	}

	public void clientVerwijderd()
	{
	}

}
