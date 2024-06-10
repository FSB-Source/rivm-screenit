package nl.rivm.screenit.model.cervix;

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

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

@Getter
@Setter
public class CervixLabformulierenFilter implements Serializable
{
	private static final long serialVersionUID = 1L;

	private Long instellingId;

	private OrganisatieType organisatieType;

	private LabprocesStap labprocesStap;

	private String monsterId;

	private String bsn;

	private List<CervixLabformulierStatus> labformulierStatussen;

	private Date scanDatumVanaf;

	private Date scanDatumTotEnMet;

	private Date geboortedatum;

	public enum LabprocesStap
	{
		CONTROLEREN,
		CONTROLEREN_VOOR_CYTOLOGIE,
		HUISARTS_ONBEKEND,
		CYTOLOGIE
	}

	private Boolean digitaal;

	public CervixLabformulierenFilter(Long instellingId, OrganisatieType organisatieType, LabprocesStap labprocesStap, String bsn,
		List<CervixLabformulierStatus> labformulierStatussen,
		Date scanDatumVanaf, Date scanDatumTotEnMet, String monsterId, Date geboortedatum, Boolean digitaal)
	{
		this.instellingId = instellingId;
		this.organisatieType = organisatieType;
		this.labprocesStap = labprocesStap;
		this.monsterId = monsterId;
		this.bsn = bsn;
		this.labformulierStatussen = labformulierStatussen;
		this.scanDatumVanaf = scanDatumVanaf;
		this.scanDatumTotEnMet = scanDatumTotEnMet;
		this.geboortedatum = geboortedatum;
		this.digitaal = digitaal;
	}

	@Override
	public String toString()
	{
		return ToStringBuilder.reflectionToString(this, ToStringStyle.SHORT_PREFIX_STYLE);
	}

}
