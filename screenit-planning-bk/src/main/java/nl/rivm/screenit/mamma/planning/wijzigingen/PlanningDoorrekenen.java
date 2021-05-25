package nl.rivm.screenit.mamma.planning.wijzigingen;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.time.LocalDate;

import nl.rivm.screenit.mamma.planning.model.PlanningBenodigd;
import nl.rivm.screenit.mamma.planning.model.PlanningClient;
import nl.rivm.screenit.mamma.planning.model.PlanningConstanten;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeks;
import nl.rivm.screenit.mamma.planning.model.PlanningPostcodeReeksRegio;
import nl.rivm.screenit.mamma.planning.model.PlanningStandplaats;
import nl.rivm.screenit.mamma.planning.model.PlanningTehuis;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public enum PlanningDoorrekenen
{
	;

	private static final Logger LOG = LoggerFactory.getLogger(PlanningDoorrekenen.class);

	static void run(PlanningClient client)
	{
		LOG.trace("run client: " + client.getId());

		PlanningBenodigd benodigd = client.getBenodigd();
		benodigd.clear();

		BigDecimal benodigdeCapaciteit = client.getBenodigdeCapaciteit(client.getScreeningsOrganisatie());

		for (int jaar = client.getVanafJaar(); jaar <= client.getTotEnMetJaar(); jaar++)
		{
			benodigd.get(jaar).add(benodigdeCapaciteit, client.getTehuis() != null);
		}

		if (client.getUitnodigenVanafJaar() > PlanningConstanten.plannenVanafJaar && client.getUitnodigenVanafJaar() <= PlanningConstanten.plannenTotEnMetJaar)
		{
			benodigd.get(client.getUitnodigenVanafJaar()).addToNieuw(benodigdeCapaciteit, client.getTehuis() != null);
		}

		if (client.getUitnodigenTotEnMetJaar() >= PlanningConstanten.plannenVanafJaar && client.getUitnodigenTotEnMetJaar() <= PlanningConstanten.plannenTotEnMetJaar)
		{
			benodigd.get(client.getUitnodigenTotEnMetJaar()).addToOud(benodigdeCapaciteit, client.getTehuis() != null);
		}

		if (client.getFactorType() == MammaFactorType.EERSTE_ONDERZOEK)
		{

			int eersteOnderzoekCorrectieVanafJaar;
			if (client.getUitnodigenVanafJaar() < PlanningConstanten.plannenVanafJaar)
			{

				eersteOnderzoekCorrectieVanafJaar = client.getVanafJaar();
			}
			else
			{

				eersteOnderzoekCorrectieVanafJaar = client.getUitnodigenVanafJaar() + 2;
			}

			for (int jaar = eersteOnderzoekCorrectieVanafJaar; jaar <= client.getTotEnMetJaar(); jaar++)
			{
				benodigd.get(jaar).addToEersteOnderzoekCorrectie(client.getDeelnamekans().subtract(benodigdeCapaciteit));
			}
		}
	}

	static void run(PlanningPostcodeReeksRegio postcodeReeksRegio)
	{
		LOG.trace("run postcodeReeksRegio: " + postcodeReeksRegio.getCijfer());

		PlanningBenodigd benodigd = postcodeReeksRegio.getBenodigd();
		benodigd.clear();

		for (PlanningClient client : postcodeReeksRegio.getClientSet())
		{
			benodigd.add(client.getBenodigd());
		}
	}

	static void run(PlanningPostcodeReeks postcodeReeks)
	{
		LOG.trace("run postcodeReeks: " + postcodeReeks.getVanPostcode());

		PlanningBenodigd benodigd = postcodeReeks.getBenodigd();
		benodigd.clear();

		for (PlanningPostcodeReeksRegio postcodeReeksRegio : postcodeReeks.getPostcodeReeksRegios())
		{
			benodigd.add(postcodeReeksRegio.getBenodigd());
		}
	}

	static void run(PlanningTehuis tehuis)
	{
		LOG.trace("run tehuis: " + tehuis.getId());

		PlanningBenodigd benodigd = tehuis.getBenodigd();
		benodigd.clear();

		for (PlanningClient client : tehuis.getClientSet())
		{
			benodigd.add(client.getBenodigd());
		}
	}

	static void run(PlanningStandplaats standplaats)
	{
		LOG.debug("run standplaats: " + standplaats.getId());

		PlanningBenodigd benodigd = standplaats.getBenodigd();
		benodigd.clear();
		for (PlanningPostcodeReeks postcodeReeks : standplaats.getPostcodeReeksSet())
		{
			benodigd.add(postcodeReeks.getBenodigd());
		}
		for (PlanningTehuis tehuis : standplaats.getTehuisSet())
		{
			benodigd.add(tehuis.getBenodigd());
		}

		PlanningBenodigd transport = standplaats.getTransport();
		transport.clear();
		for (PlanningClient client : standplaats.getTransportVanSet())
		{
			transport.subtract(client.getBenodigd());
		}
		for (PlanningClient client : standplaats.getTransportNaarSet())
		{
			transport.add(client.getBenodigd());
		}

		long somDatum = 0L;
		long aantalScreeningRonden = 0;
		for (PlanningPostcodeReeks postcodeReeks : standplaats.getPostcodeReeksSet())
		{
			for (PlanningPostcodeReeksRegio postcodeReeksRegio : postcodeReeks.getPostcodeReeksRegios())
			{
				for (PlanningClient client : postcodeReeksRegio.getClientSet())
				{
					LocalDate vorigeScreeningRondeCreatieDatum = client.getVorigeScreeningRondeCreatieDatum();
					if (vorigeScreeningRondeCreatieDatum != null)
					{
						somDatum += vorigeScreeningRondeCreatieDatum.toEpochDay();
						aantalScreeningRonden++;
					}
				}
			}
		}

		standplaats.setVorigeGewogenGemiddeldeDatum(aantalScreeningRonden != 0 ? LocalDate.ofEpochDay(somDatum / aantalScreeningRonden) : null);
	}
}
