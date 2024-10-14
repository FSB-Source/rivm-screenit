package nl.rivm.screenit.model;

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

import java.util.HashMap;
import java.util.Map;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.overeenkomsten.AbstractAfgeslotenOvereenkomst;
import nl.rivm.screenit.model.project.ProjectAttribuut;
import nl.rivm.screenit.model.project.ProjectBrief;

@Getter
@Setter
public class MailMergeContext
{

	public static final String CONTEXT_CERVIX_HUISARTS = "BMHK_HUISARTS";

	public static final String CONTEXT_MAMMA_CE = "BK_CE";

	public static final String CONTEXT_HA_LAB_FORM_VOLGNUMMER = "HA_LAB_FORM_VOLGNUMMER";

	public static final String CONTEXT_HA_LOCATIE = "HA_LOCATIE";

	public static final String CONTEXT_HA_AANTAL_FORM = "HA_AANTAL_FORM";

	public static final String CONTEXT_SCREENING_ORGANISATIE = "SCREENING_ORGANISATIE";

	public static final String CONTEXT_BMHK_BETAALOPDRACHT = "BMHK_BETAALOPDRACHT";

	public static final String CONTEXT_MAMMA_BEOORDELING = "BK_BEOORDELING";

	public static final String CONTEXT_MAMMA_TOON_LOCATIE_WIJZIGING_TEKST = "BK_TOON_LOCATIE_WIJZIGING_TEKST";

	private boolean useTestValue = false;

	private Client client;

	private ColonIntakelocatie intakelocatie;

	private ColonUitnodiging colonUitnodiging;

	private ColonIntakeAfspraak intakeAfspraak;

	private ColonIntakeAfspraak vorigeIntakeAfspraak;

	private AbstractAfgeslotenOvereenkomst overeenkomst;

	private ProjectBrief projectBrief;

	private String vragenlijstNaam;

	private CervixUitnodiging cervixUitnodiging;

	private BMHKLaboratorium bmhkLaboratorium;

	private Map<ProjectAttribuut, String> projectAttributen = new HashMap<>();

	@Setter(AccessLevel.NONE)
	@Getter(AccessLevel.NONE)
	private Map<String, Object> contextMap = new HashMap<>();

	private Brief brief;

	@Override
	public int hashCode()
	{
		var prime = 31;
		var result = 1;
		result = prime * result + (client == null ? 0 : client.hashCode());
		result = prime * result + (colonUitnodiging == null ? 0 : colonUitnodiging.hashCode());
		result = prime * result + (intakeAfspraak == null ? 0 : intakeAfspraak.hashCode());
		result = prime * result + (overeenkomst == null ? 0 : overeenkomst.hashCode());
		result = prime * result + (vorigeIntakeAfspraak == null ? 0 : vorigeIntakeAfspraak.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (obj == null)
		{
			return false;
		}
		if (getClass() != obj.getClass())
		{
			return false;
		}
		var other = (MailMergeContext) obj;
		if (client == null)
		{
			if (other.client != null)
			{
				return false;
			}
		}
		else if (!client.equals(other.client))
		{
			return false;
		}
		if (colonUitnodiging == null)
		{
			if (other.colonUitnodiging != null)
			{
				return false;
			}
		}
		else if (!colonUitnodiging.equals(other.colonUitnodiging))
		{
			return false;
		}
		if (intakeAfspraak == null)
		{
			if (other.intakeAfspraak != null)
			{
				return false;
			}
		}
		else if (!intakeAfspraak.equals(other.intakeAfspraak))
		{
			return false;
		}
		if (overeenkomst == null)
		{
			if (other.overeenkomst != null)
			{
				return false;
			}
		}
		else if (!overeenkomst.equals(other.overeenkomst))
		{
			return false;
		}
		if (vorigeIntakeAfspraak == null)
		{
			if (other.vorigeIntakeAfspraak != null)
			{
				return false;
			}
		}
		else if (!vorigeIntakeAfspraak.equals(other.vorigeIntakeAfspraak))
		{
			return false;
		}
		return true;
	}

	public void putValue(String name, Object value)
	{
		contextMap.put(name, value);
	}

	@SuppressWarnings("unchecked")
	public <T> T getValue(String name)
	{
		return (T) contextMap.get(name);
	}
}
