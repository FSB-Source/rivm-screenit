
package nl.rivm.screenit.model.logging;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.RetourzendingAfhandelingType;

import org.apache.commons.lang.StringUtils;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;

@Entity
@Table(schema = "gedeeld")
public class RetourzendingLogEvent extends LogEvent
{
	@OneToOne(fetch = FetchType.LAZY)
	@Cascade(CascadeType.SAVE_UPDATE)
	private UploadDocument sanddBestand;

	private Integer nieuweGbaAanvraagRegels = 0;

	private Integer directNieuweUitnodigingRegels = 0;

	private Integer geweigerdBriefRegels = 0;

	private Integer skippedRegels = 0;

	private String trackIdMelding;

	private String retourRedenMelding;

	private String zendingHeeftAlRetourstatusMelding;

	private String clientNietGevondenMelding;

	private String rondeNietActiefMelding;

	private String uitnodigingNietValideMelding;

	public void incrTrackIdNietGevondenRegels(int regelNr)
	{
		if (StringUtils.isBlank(trackIdMelding))
		{
			trackIdMelding = "TrackID niet gevonden (regelnummers): " + regelNr;
		}
		else
		{
			trackIdMelding += "," + regelNr;
		}
	}

	public void incrGeenValideUitnodiging(int regelNr)
	{
		if (StringUtils.isBlank(uitnodigingNietValideMelding))
		{
			uitnodigingNietValideMelding = "Uitnodigingen niet valide (regelnummers): " + regelNr;
		}
		else
		{
			uitnodigingNietValideMelding += "," + regelNr;
		}
	}

	public void incrGeenRondeActiefRegels(int regelNr)
	{
		if (StringUtils.isBlank(rondeNietActiefMelding))
		{
			rondeNietActiefMelding = "Ronde of dossier niet actief (regelnummers): " + regelNr;
		}
		else
		{
			rondeNietActiefMelding += "," + regelNr;
		}
	}

	public void incrZendingHeeftAlRetourstatusRegels(int regelNr)
	{
		if (StringUtils.isBlank(zendingHeeftAlRetourstatusMelding))
		{
			zendingHeeftAlRetourstatusMelding = "Zending heeft al retourstatus (regelnummers): " + regelNr;
		}
		else
		{
			zendingHeeftAlRetourstatusMelding += "," + regelNr;
		}
	}

	public void incrRetourRedenNietGevondenRegels(int regelNr)
	{
		if (StringUtils.isBlank(retourRedenMelding))
		{
			retourRedenMelding = "Retourreden niet gevonden (regelnummers): " + regelNr;
		}
		else
		{
			retourRedenMelding += "," + regelNr;
		}
	}

	public void incrClientNietGevondenRegels(int regelNr)
	{
		if (StringUtils.isBlank(clientNietGevondenMelding))
		{
			clientNietGevondenMelding = "Client niet gevonden (regelnummers): " + regelNr;
		}
		else
		{
			clientNietGevondenMelding += "," + regelNr;
		}
	}

	public void incrSkippedRegels()
	{
		setLevel(Level.WARNING);
		skippedRegels++;
		String melding = "Aantal overgeslagen: " + skippedRegels;
		setMelding(melding);
	}

	public UploadDocument getSanddBestand()
	{
		return sanddBestand;
	}

	public void setSanddBestand(UploadDocument sanddBestand)
	{
		this.sanddBestand = sanddBestand;
	}

	public void incrRegels(RetourzendingAfhandelingType afhandelingType)
	{
		switch (afhandelingType)
		{
		case NIEUWE_GBA_AANVRAAG:
			nieuweGbaAanvraagRegels++;
			break;
		case DIRECT_NIEUWE_UITNODIGING:
			directNieuweUitnodigingRegels++;
			break;
		case GEWEIGERD:
			geweigerdBriefRegels++;
			break;
		default:
			break;
		}
	}
}
