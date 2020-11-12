package nl.rivm.screenit.mamma.se.service.dtomapper;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.mamma.se.dto.LezingSeDto;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.util.NaamUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LezingDtoMapper
{
	private static final Logger LOG = LoggerFactory.getLogger(LezingDtoMapper.class);

	private DoorsnedeAfbeeldingenDtoMapper doorsnedeAfbeeldingenDtoMapper = new DoorsnedeAfbeeldingenDtoMapper();

	public List<LezingSeDto> createLezingDtos(long uitnodigingsNr, MammaBeoordeling beoordeling)
	{
		List<LezingSeDto> lezingen = new ArrayList<>();

		if (beoordeling != null && beoordeling.getEersteLezing() != null)
		{
			if (beoordeling.getEersteLezing().getBeoordelaar() != null)
			{
				lezingen.add(createLezingDto(beoordeling.getEersteLezing()));
			}
			else
			{
				LOG.error("Voor uitnodigingsNr: " + uitnodigingsNr + ", en beoordeling id: " + beoordeling.getId() + " getEersteLezing().getBeoordelaar() == null");
			}
		}
		else if (beoordeling != null)
		{
			LOG.error("Voor uitnodigingsNr: " + uitnodigingsNr + ", en beoordeling id: " + beoordeling.getId() + " getEersteLezing() == null");
		}
		else
		{
			LOG.error("Voor uitnodigingsNr: " + uitnodigingsNr + " Null beoordeling");
			return lezingen;
		}

		if (beoordeling.getTweedeLezing() != null)
		{
			if (beoordeling.getTweedeLezing().getBeoordelaar() != null)
			{
				lezingen.add(createLezingDto(beoordeling.getTweedeLezing()));
			}
			else
			{
				LOG.error("Voor uitnodigingsNr: " + uitnodigingsNr + ", en beoordeling id: " + beoordeling.getId() + " getTweedeLezing().getBeoordelaar() == null");
			}
		}
		else
		{
			LOG.error("Voor uitnodigingsNr: " + uitnodigingsNr + ", en beoordeling id: " + beoordeling.getId() + " getTweedeLezing() == null");
		}

		if (beoordeling.getArbitrageLezing() != null)
		{
			if (beoordeling.getArbitrageLezing().getBeoordelaar() != null)
			{
				lezingen.add(createLezingDto(beoordeling.getArbitrageLezing()));
			}
			else
			{
				LOG.error("Voor uitnodigingsNr: " + uitnodigingsNr + ", en beoordeling id: " + beoordeling.getId() + " getArbitrageLezing().getBeoordelaar() == null");
			}
		}
		else if (beoordeling.getDiscrepantieLezing() != null)
		{
			lezingen.add(createLezingDto(beoordeling.getDiscrepantieLezing()));
		}

		return lezingen;
	}

	private LezingSeDto createLezingDto(MammaLezing mammaLezing)
	{
		LezingSeDto lezing = new LezingSeDto();
		if (mammaLezing.getBeoordelaar() != null)
		{
			lezing.setRadioloogNaam(NaamUtil.getNaamGebruiker(mammaLezing.getBeoordelaar().getMedewerker()));
		}
		lezing.setLezingType(mammaLezing.getLezingType().getNaam());
		lezing.setBiradsRechts(mammaLezing.getBiradsRechts().getNaam());
		lezing.setBiradsLinks(mammaLezing.getBiradsLinks().getNaam());
		lezing.setLezingAanzichten(doorsnedeAfbeeldingenDtoMapper.createDoorsnedeAfbeeldingenDtoFromLaesies(mammaLezing.getLaesies()));
		return lezing;
	}
}
