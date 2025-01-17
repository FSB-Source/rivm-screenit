package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBestand;
import nl.rivm.screenit.model.project.ProjectBestandType;
import nl.rivm.screenit.model.project.ProjectBestandVerwerkingEntry;
import nl.rivm.screenit.model.project.ProjectGroep;
import nl.rivm.screenit.service.ClientService;

import org.apache.commons.lang.StringUtils;

@Slf4j
public class DefaultProjectBestandVerwerkingContext extends BaseProjectBestandVerwerkingContext
{

	private static final String BSN = "bsn";

	private static final String GEBOORTEDATUM = "geboortedatum";

	private final ProjectBestand bestand;

	private int bsnColumn = -1;

	private int geboortedatumColumn = -1;

	public DefaultProjectBestandVerwerkingContext(ProjectBestand bestand, File file) throws Exception
	{
		super(file);
		this.bestand = bestand;
	}

	@Override
	protected void addAttribuutHeaderHook()
	{
		if (bestand.getType() == ProjectBestandType.ATTRIBUTEN || bestand.getType() == ProjectBestandType.POPULATIE)
		{
			bestand.setAttributen(true);
		}
	}

	@Override
	public Collection<ProjectAttribuut> getAttributen()
	{
		return getAttributenPositie().keySet();
	}

	@Override
	protected void checkClientIdentifingHeaders()
	{
		if (bsnColumn == -1 || geboortedatumColumn == -1)
		{
			throw new IllegalStateException("Geen header 'bsn' en/of 'geboortedatum' gevonden in het bestand.");
		}
	}

	@Override
	public Project getProject()
	{
		return bestand.getProject();
	}

	@Override
	public ProjectBestandType getType()
	{
		return bestand.getType();
	}

	@Override
	public ProjectGroep getGroep()
	{
		return bestand.getGroep();
	}

	@Override
	public boolean heeftAttributen()
	{
		return bestand.isAttributen();
	}

	@Override
	public void verwerkingGeslaagd()
	{
		var verwerking = bestand.getVerwerking();
		verwerking.setRegelsVerwerkt(verwerking.getRegelsVerwerkt() + 1);
	}

	@Override
	public void verwerkingMislukt(IllegalStateException e)
	{
		addBestandsMelding(getHuidigeRegelnummer(), e.getMessage());
		var verwerking = bestand.getVerwerking();
		verwerking.setRegelsMislukt(verwerking.getRegelsMislukt() + 1);
		LOG.warn("Probleem opgetreden met verwerken van project bestand {} op regelnummer {}", bestand.getUploadDocument().getNaam(),
			getHuidigeRegelnummer());
	}

	@Override
	public void attribuutGewijzigd()
	{
		var verwerking = bestand.getVerwerking();
		verwerking.setAttributenGewijzigd(verwerking.getAttributenGewijzigd() + 1);
	}

	@Override
	public void clientGeinactiveerd()
	{
		var verwerking = bestand.getVerwerking();
		verwerking.setGeinactiveerd(verwerking.getGeinactiveerd() + 1);
	}

	@Override
	public void clientGeheractiveerd()
	{
		var verwerking = bestand.getVerwerking();
		verwerking.setGeheractiveerd(verwerking.getGeheractiveerd() + 1);
	}

	@Override
	public void clientVerwijderd()
	{
		var verwerking = bestand.getVerwerking();
		verwerking.setVerwijderd(verwerking.getVerwijderd() + 1);
	}

	@Override
	public Client getClient()
	{
		var bsn = getBsnVanHuidigeRegel();
		var geboortedatum = getGeboortedatumVanHuidigeRegel();
		var client = getBean(ClientService.class).getClientByBsn(bsn);
		if (client == null || !geboortedatum.equals(client.getPersoon().getGeboortedatum()))
		{
			throw new IllegalStateException("Client niet gevonden met opgegeven bsn en geboortedatum");
		}
		return client;
	}

	@Override
	protected boolean addClientIdentifingHeader(String header)
	{
		var gevonden = false;
		var headers = getHuidigeRegel();
		var geformateerdeHeader = header.toLowerCase().trim();
		if (BSN.equals(geformateerdeHeader))
		{
			bsnColumn = headers.indexOf(header);
			gevonden = true;
		}
		else if (GEBOORTEDATUM.equals(geformateerdeHeader))
		{
			geboortedatumColumn = headers.indexOf(header);
			gevonden = true;
		}
		return gevonden;
	}

	protected void addBestandsMelding(Integer regelnummer, String melding)
	{
		var verwerking = bestand.getVerwerking();
		var entry = new ProjectBestandVerwerkingEntry();
		entry.setRegelNummer(regelnummer);
		entry.setMelding(melding);
		entry.setVerwerking(verwerking);
		verwerking.getMeldingen().add(entry);
	}

	private Date getGeboortedatumVanHuidigeRegel() throws IllegalStateException
	{
		var value = getHuidigeRegel().get(geboortedatumColumn);
		if (StringUtils.isNotBlank(value))
		{
			var format = Constants.getDateFormat();
			try
			{
				return format.parse(value);
			}
			catch (ParseException e)
			{
				try
				{
					format = new SimpleDateFormat("d-M-yyyy");
					return format.parse(value);
				}
				catch (ParseException e1)
				{
					throw new IllegalStateException("Datum kon niet worden herkend gebruik het format, 1-1-2015 of 01-01-2015");
				}
			}
		}
		throw new IllegalStateException("Datum kon niet worden herkend gebruik het format, 1-1-2015 of 01-01-2015");
	}

	private String getBsnVanHuidigeRegel()
	{
		return getHuidigeRegel().get(bsnColumn);
	}

}
