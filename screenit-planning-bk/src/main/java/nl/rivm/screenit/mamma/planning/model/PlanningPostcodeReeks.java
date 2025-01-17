package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class PlanningPostcodeReeks extends PlanningEntiteit
{
	private String vanPostcode;

	private String totPostcode;

	private Set<String> postcodeSet = new HashSet<>();

	private PlanningStandplaats standplaats;

	private final Map<String, PlanningPostcodeReeksRegio> postcodeReeksRegioMap = new HashMap<>();

	private final PlanningBenodigd benodigd = new PlanningBenodigd();

	public PlanningPostcodeReeks(Long id, String vanPostcode, String totPostcode)
	{
		super(id);
		setPostcodeReeks(vanPostcode, totPostcode);
	}

	public void setPostcodeReeks(String vanPostcode, String totPostcode)
	{
		postcodeSet = new HashSet<>();

		this.vanPostcode = vanPostcode;
		this.totPostcode = totPostcode;

		Integer vanCijfer = Integer.parseInt(vanPostcode.substring(0, 4));
		Integer maxCijfer = Integer.parseInt(totPostcode.substring(0, 4));

		char vanLetter1 = vanPostcode.charAt(4);
		char totLetter1 = totPostcode.charAt(4);
		char vanLetter2 = vanPostcode.charAt(5);
		char totLetter2 = totPostcode.charAt(5);

		for (int cijfer = vanCijfer; cijfer <= maxCijfer; cijfer++)
		{
			char letter1 = cijfer == vanCijfer ? vanLetter1 : 'A';
			char maxLetter1 = cijfer == maxCijfer ? totLetter1 : 'Z';

			for (; letter1 <= maxLetter1; letter1++)
			{
				char letter2 = cijfer == vanCijfer && letter1 == vanLetter1 ? vanLetter2 : 'A';
				char maxLetter2 = cijfer == maxCijfer && letter1 == totLetter1 ? totLetter2 : 'Z';

				for (; letter2 <= maxLetter2; letter2++)
				{
					postcodeSet.add(String.valueOf(cijfer) + letter1 + letter2);
				}
			}
		}
	}

	public void setStandplaats(PlanningStandplaats standplaats)
	{
		this.standplaats = standplaats;
	}

	public String getVanPostcode()
	{
		return vanPostcode;
	}

	public String getTotPostcode()
	{
		return totPostcode;
	}

	public boolean inPostcodeReeks(PlanningClient client)
	{
		String postcode = client.getPostcode();
		return postcode.compareTo(vanPostcode) >= 0 && postcode.compareTo(totPostcode) <= 0;
	}

	public PlanningPostcodeReeksRegio getPostcodeReeksRegio(PlanningClient client)
	{
		String postcodeCijfer = client.getPostcode().substring(0, 4);
		PlanningPostcodeReeksRegio postcodeReeksRegio = postcodeReeksRegioMap.get(postcodeCijfer);
		if (postcodeReeksRegio == null)
		{
			postcodeReeksRegio = new PlanningPostcodeReeksRegio(postcodeCijfer);
			postcodeReeksRegioMap.put(postcodeCijfer, postcodeReeksRegio);
		}
		return postcodeReeksRegio;
	}

	public Collection<PlanningPostcodeReeksRegio> getPostcodeReeksRegios()
	{
		return postcodeReeksRegioMap.values();
	}

	public void removePostcodeReeksRegios(Collection<String> postcodeReeksRegioCijfers)
	{
		postcodeReeksRegioMap.keySet().removeAll(postcodeReeksRegioCijfers);
	}

	public Set<String> getPostcodeSet()
	{
		return postcodeSet;
	}

	public PlanningStandplaats getStandplaats()
	{
		return standplaats;
	}

	public PlanningBenodigd getBenodigd()
	{
		return benodigd;
	}
}
