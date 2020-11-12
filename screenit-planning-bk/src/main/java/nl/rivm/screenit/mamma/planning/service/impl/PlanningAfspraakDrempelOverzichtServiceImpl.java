package nl.rivm.screenit.mamma.planning.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import nl.rivm.screenit.dto.mamma.planning.PlanningAfspraakDrempelOverzichtDto;
import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeks;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeksRegio;
import nl.rivm.screenit.mamma.planning.model.PlanningScreeningsOrganisatie;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningTehuis;
import nl.rivm.screenit.mamma.planning.service.PlanningAfspraakDrempelOverzichtService;
import nl.rivm.screenit.service.ICurrentDateSupplier;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class PlanningAfspraakDrempelOverzichtServiceImpl implements PlanningAfspraakDrempelOverzichtService
{
	@Autowired
	private ICurrentDateSupplier dateSupplier;

	private static final int MAX_CDV_INDEX = 100;

	public PlanningAfspraakDrempelOverzichtDto getAfspraakDrempelOverzicht(PlanningStandplaats standplaats)
	{
		Set<PlanningClient> clientSet = new HashSet<>();

		for (PlanningPostcodeReeks postcodeReeks : standplaats.getPostcodeReeksSet())
		{
			for (PlanningPostcodeReeksRegio postcodeReeksRegio : postcodeReeks.getPostcodeReeksRegios())
			{
				clientSet.addAll(postcodeReeksRegio.getClientSet());
			}
		}

		for (PlanningTehuis tehuis : standplaats.getTehuisSet())
		{
			clientSet.addAll(tehuis.getClientSet());
		}

		return getAfspraakDrempelOverzicht(clientSet);
	}

	public PlanningAfspraakDrempelOverzichtDto getAfspraakDrempelOverzicht(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		return getAfspraakDrempelOverzicht(screeningsOrganisatie.getClientList());
	}

	private PlanningAfspraakDrempelOverzichtDto getAfspraakDrempelOverzicht(Collection<PlanningClient> clientCollection)
	{
		int uitnodigenJaar = dateSupplier.getLocalDate().getYear();

		PlanningAfspraakDrempelOverzichtDto deelnamekansDto = new PlanningAfspraakDrempelOverzichtDto();
		deelnamekansDto.vanafGeboortejaar = uitnodigenJaar - PlanningConstanten.totEnMetLeeftijd;
		deelnamekansDto.totEnMetGeboortejaar = uitnodigenJaar - PlanningConstanten.vanafLeeftijd;

		clientCollection = clientCollection.stream()
			.filter(client -> client.getUitnodigenVanafJaar() <= uitnodigenJaar && uitnodigenJaar <= client.getUitnodigenTotEnMetJaar())
			.collect(Collectors.toList());

		Long[] cdvTotaal = nieuweCumulatieveDeelnamekansVerdeling();
		Long[] cdvVervolgRonde = nieuweCumulatieveDeelnamekansVerdeling();
		Long[] cdvEersteRonde = nieuweCumulatieveDeelnamekansVerdeling();
		Long[] cdvDubbeleTijd = nieuweCumulatieveDeelnamekansVerdeling();
		Long[] cdvMinderValide = nieuweCumulatieveDeelnamekansVerdeling();
		Long[] cdvTehuis = nieuweCumulatieveDeelnamekansVerdeling();
		Long[] cdvSuspect = nieuweCumulatieveDeelnamekansVerdeling();

		Long[] cdvDrempelToepassenTotaal = nieuweCumulatieveDeelnamekansVerdeling();
		Long[] cdvDrempelToepassenVervolgRonde = nieuweCumulatieveDeelnamekansVerdeling();
		Long[] cdvDrempelToepassenEersteRonde = nieuweCumulatieveDeelnamekansVerdeling();
		Long[] cdvDrempelToepassenDubbeleTijd = nieuweCumulatieveDeelnamekansVerdeling();

		BigDecimal somDeelnamekansenVervolgRonde = BigDecimal.ZERO;
		BigDecimal somDeelnamekansenEersteRonde = BigDecimal.ZERO;

		for (PlanningClient client : clientCollection)
		{
			int cdvIndex = client.getDeelnamekans().movePointRight(2).intValue();
			boolean drempelToepassen = true;

			if (client.getTehuis() != null)
			{
				voegToeCumulatieveVerdeling(cdvTehuis, cdvIndex);
				drempelToepassen = false;
			}

			if (client.isSuspect())
			{
				voegToeCumulatieveVerdeling(cdvSuspect, cdvIndex);
				drempelToepassen = false;
			}

			switch (client.getDoelgroep())
			{
			case MINDER_VALIDE:
				voegToeCumulatieveVerdeling(cdvMinderValide, cdvIndex);
				drempelToepassen = false;
				break;
			case DUBBELE_TIJD:
				voegToeCumulatieveVerdeling(cdvDubbeleTijd, cdvIndex);
				if (drempelToepassen)
				{
					voegToeCumulatieveVerdeling(cdvDrempelToepassenDubbeleTijd, cdvIndex);
				}
				break;
			}

			voegToeCumulatieveVerdeling(cdvTotaal, cdvIndex);
			if (drempelToepassen)
			{
				voegToeCumulatieveVerdeling(cdvDrempelToepassenTotaal, cdvIndex);
			}

			if (client.getDeelnamekansVervolgRonde())
			{
				voegToeCumulatieveVerdeling(cdvVervolgRonde, cdvIndex);
				if (drempelToepassen)
				{
					voegToeCumulatieveVerdeling(cdvDrempelToepassenVervolgRonde, cdvIndex);
				}
				somDeelnamekansenVervolgRonde = somDeelnamekansenVervolgRonde.add(client.getDeelnamekans());
			}
			else
			{
				voegToeCumulatieveVerdeling(cdvEersteRonde, cdvIndex);
				if (drempelToepassen)
				{
					voegToeCumulatieveVerdeling(cdvDrempelToepassenEersteRonde, cdvIndex);
				}
				somDeelnamekansenEersteRonde = somDeelnamekansenEersteRonde.add(client.getDeelnamekans());
			}
		}

		deelnamekansDto.gemiddeldeDeelnamekansVervolgRonde = cdvVervolgRonde[MAX_CDV_INDEX] > 0
			? somDeelnamekansenVervolgRonde.divide(new BigDecimal(cdvVervolgRonde[MAX_CDV_INDEX]), 2, RoundingMode.HALF_UP).movePointRight(2)
			: BigDecimal.ZERO;
		deelnamekansDto.gemiddeldeDeelnamekansEersteRonde = cdvEersteRonde[MAX_CDV_INDEX] > 0
			? somDeelnamekansenEersteRonde.divide(new BigDecimal(cdvEersteRonde[MAX_CDV_INDEX]), 2, RoundingMode.HALF_UP).movePointRight(2)
			: BigDecimal.ZERO;

		deelnamekansDto.kolommen = new PlanningAfspraakDrempelOverzichtDto.Kolom[] {
			new PlanningAfspraakDrempelOverzichtDto.Kolom("Totaal", cdvTotaal, cdvDrempelToepassenTotaal),
			new PlanningAfspraakDrempelOverzichtDto.Kolom("Vervolgronde", cdvVervolgRonde, cdvDrempelToepassenVervolgRonde),
			new PlanningAfspraakDrempelOverzichtDto.Kolom("Eerste ronde", cdvEersteRonde, cdvDrempelToepassenEersteRonde),
			new PlanningAfspraakDrempelOverzichtDto.Kolom("Dubbele tijd", cdvDubbeleTijd, cdvDrempelToepassenDubbeleTijd),
			new PlanningAfspraakDrempelOverzichtDto.Kolom("Minder valide", cdvMinderValide, null),
			new PlanningAfspraakDrempelOverzichtDto.Kolom("Tehuis", cdvTehuis, null),
			new PlanningAfspraakDrempelOverzichtDto.Kolom("Suspect", cdvSuspect, null),
		};

		return deelnamekansDto;
	}

	private Long[] nieuweCumulatieveDeelnamekansVerdeling()
	{
		Long[] cumulatieveVerdeling = new Long[MAX_CDV_INDEX + 1];
		Arrays.fill(cumulatieveVerdeling, 0L);
		return cumulatieveVerdeling;
	}

	private void voegToeCumulatieveVerdeling(Long[] cumulatieveDeelnamekansVerdeling, int cumulatieveDeelnamekansVerdelingIndex)
	{
		for (int i = cumulatieveDeelnamekansVerdelingIndex; i <= MAX_CDV_INDEX; i++)
		{
			cumulatieveDeelnamekansVerdeling[i]++;
		}
	}
}
