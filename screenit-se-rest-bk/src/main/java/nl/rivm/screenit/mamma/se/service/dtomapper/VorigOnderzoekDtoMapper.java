package nl.rivm.screenit.mamma.se.service.dtomapper;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.se.dto.onderzoek.VorigOnderzoekDto;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingOpschortenReden;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnderbrokenOnderzoekOption;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.NaamUtil;

public class VorigOnderzoekDtoMapper
{
	private OnderzoekDtoMapper onderzoekDtoMapper = new OnderzoekDtoMapper();

	private AfbeeldingDtoMapper afbeeldingDtoMapper = new AfbeeldingDtoMapper();

	private LezingDtoMapper lezingDtoMapper = new LezingDtoMapper();

	private VerslagLezingDtoMapper verslagLezingDtoMapper = new VerslagLezingDtoMapper();

	private SignalerenDtoMapper signalerenDtoMapper = new SignalerenDtoMapper();

	public VorigOnderzoekDto createVorigOnderzoekDto(MammaScreeningRonde ronde, MammaBaseBeoordelingService beoordelingService, MammaBaseOnderzoekService mammaBaseOnderzoekService)
	{
		VorigOnderzoekDto vorigOnderzoekDto = new VorigOnderzoekDto();

		MammaOnderzoek onderzoek = ronde.getLaatsteOnderzoek();
		if (onderzoek != null && onderzoek.isDoorgevoerd())
		{
			MammaMammografie mammografie = onderzoek.getMammografie();
			vorigOnderzoekDto.setUitnodigingsNr(ronde.getUitnodigingsNr());
			vorigOnderzoekDto.setEersteBeeindigdeAfspraakOp(eersteBeeindigdeAfspraakOp(ronde));
			vorigOnderzoekDto.setOnderzoekDatum(DateUtil.toLocalDateTime(onderzoek.getCreatieDatum()));
			vorigOnderzoekDto.setUitvoerendMbber(
				NaamUtil.getNaamGebruiker(mammografie != null && mammografie.getAfgerondDoor() != null ? mammografie.getAfgerondDoor().getMedewerker() : null));
			vorigOnderzoekDto.setExtraMedewerker(onderzoek.getExtraMedewerker() == null ? null : NaamUtil.getNaamGebruiker(onderzoek.getExtraMedewerker().getMedewerker()));
			vorigOnderzoekDto.setMeerdereOnderzoekenInRondeOnderbrokenRedenen(geefRedenMeerdereRondeWanneerHetGeval(ronde));
			vorigOnderzoekDto.setMeerdereOnderzoekenInRondeOpschortRedenen(geefOpschortRedenMeerdereRondeWanneerHetGeval(ronde));
			vorigOnderzoekDto.setOnderzoek(onderzoekDtoMapper.createOnderzoekDto(onderzoek));
			vorigOnderzoekDto.setBeeldenBeschikbaar(MammaMammografieIlmStatus.beeldenBeschikbaar(mammografie != null ? mammografie.getIlmStatus() : null));
			vorigOnderzoekDto.setVisueleInspectieAfbeelding(
				afbeeldingDtoMapper.createAfbeeldingDto(mammografie != null ? mammografie.getVisueleInspectieAfbeelding() : null));
			vorigOnderzoekDto.setSignaleren(signalerenDtoMapper.createSignalerenDto(onderzoek));
			vorigOnderzoekDto.setTeksten(mammaBaseOnderzoekService.vorigeRondeTeksten(onderzoek, true));

			mapVorigeBeoordelingMetUitslag(vorigOnderzoekDto, onderzoek, ronde, beoordelingService);
		}
		return vorigOnderzoekDto;
	}

	private void mapVorigeBeoordelingMetUitslag(VorigOnderzoekDto vorigOnderzoekDto, MammaOnderzoek onderzoek, MammaScreeningRonde ronde,
		MammaBaseBeoordelingService beoordelingService)
	{
		if (onderzoek.getLaatsteBeoordeling() != null && MammaBeoordelingStatus.isUitslagStatus(onderzoek.getLaatsteBeoordeling().getStatus()))
		{
			vorigOnderzoekDto.setUitslagGunstig(beoordelingService.isUitslagGunstig(onderzoek.getLaatsteBeoordeling()));
			vorigOnderzoekDto.setOnbeoordeelbaar(MammaBeoordelingStatus.ONBEOORDEELBAAR.equals(onderzoek.getLaatsteBeoordeling().getStatus()));
			vorigOnderzoekDto.setLezingen(lezingDtoMapper.createLezingDtos(ronde.getUitnodigingsNr(), onderzoek.getLaatsteBeoordeling()));
			vorigOnderzoekDto.setVerslagLezing(verslagLezingDtoMapper.createVerslagLezingDto(onderzoek.getLaatsteBeoordeling(), beoordelingService));
			vorigOnderzoekDto.setNevenbevindingen(
				onderzoek.getLaatsteBeoordeling() != null
					? beoordelingService.getMammaLezingEnumsTekst(MammaLezing::getNevenbevindingen, onderzoek.getLaatsteBeoordeling().getEersteLezing(),
						onderzoek.getLaatsteBeoordeling().getTweedeLezing())
					: "");
			vorigOnderzoekDto.setNevenbevindingenOpmerkingen(beoordelingService.getNevenBevindingenOpmerkingenAsList(onderzoek.getLaatsteBeoordeling()));
		}
	}

	private List<OnderbrokenOnderzoekOption> geefRedenMeerdereRondeWanneerHetGeval(MammaScreeningRonde ronde)
	{
		return ronde.getUitnodigingen().stream().flatMap(uitnodiging -> uitnodiging.getAfspraken().stream().filter(afspraak -> afspraak.getOnderzoek() != null))
			.filter(afspraak -> MammaOnderzoekStatus.ONDERBROKEN.equals(afspraak.getOnderzoek().getStatus()))
			.map(afspraak -> afspraak.getOnderzoek().getOnderbrokenOnderzoek()).distinct()
			.collect(Collectors.toList());
	}

	private List<MammaBeoordelingOpschortenReden> geefOpschortRedenMeerdereRondeWanneerHetGeval(MammaScreeningRonde ronde)
	{
		return ronde.getUitnodigingen().stream().flatMap(uitnodiging -> uitnodiging.getAfspraken().stream().filter(afspraak -> afspraak.getOnderzoek() != null))
			.map(MammaAfspraak::getOnderzoek)
			.filter(onderzoek -> !MammaOnderzoekStatus.AFGEROND.equals(onderzoek.getStatus()))
			.map(onderzoek -> isOpschorten(onderzoek) ? onderzoek.getLaatsteBeoordeling().getOpschortReden() : null)
			.collect(Collectors.toList());
	}

	private boolean isOpschorten(MammaOnderzoek onderzoek)
	{
		if (onderzoek.getLaatsteBeoordeling() == null || !MammaBeoordelingStatus.GEANNULEERD.equals(onderzoek.getLaatsteBeoordeling().getStatus()))
		{
			return false;
		}
		return onderzoek.getLaatsteBeoordeling().getOpschortReden() != null
			&& !MammaBeoordelingOpschortenReden.NIET_OPSCHORTEN.equals(onderzoek.getLaatsteBeoordeling().getOpschortReden());
	}

	private LocalDateTime eersteBeeindigdeAfspraakOp(MammaScreeningRonde ronde)
	{
		LocalDateTime result = null;
		for (MammaUitnodiging uitnodiging : ronde.getUitnodigingen())
		{
			for (MammaAfspraak afspraak : uitnodiging.getAfspraken())
			{
				if (afspraak.getStatus() == MammaAfspraakStatus.BEEINDIGD)
				{
					LocalDateTime afspraakVanaf = DateUtil.toLocalDateTime(afspraak.getVanaf());
					if (result == null || afspraakVanaf.isBefore(result))
					{
						result = afspraakVanaf;
					}
				}
			}
		}
		if (result == null)
		{
			throw new IllegalStateException("Vorige ronde id " + ronde.getId() + " met status " + ronde.getStatus() + " heeft wel een onderzoek maar geen beëindigde afspraak");
		}
		return result;
	}
}
