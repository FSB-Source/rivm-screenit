package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.time.LocalDate;

import javax.persistence.criteria.From;
import javax.persistence.criteria.Join;

import nl.rivm.screenit.dto.mamma.MammaLezingRapportageDto;
import nl.rivm.screenit.main.service.mamma.MammaLezingService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Termijn;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.repository.mamma.MammaBeoordelingRepository;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.PercentageUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus.UITSLAG_ONGUNSTIG;
import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftEersteOfTweedeLezingGedaanBinnenTermijn;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftStatus;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.heeftTotDiscrepantieGeleid;
import static nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification.isGedaanBinnenTermijnDoor;
import static nl.rivm.screenit.specification.mamma.MammaScreeningRondeSpecification.heeftPreciesEenRonde;
import static org.springframework.data.jpa.domain.Specification.not;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaLezingServiceImpl implements MammaLezingService
{
	@Autowired
	private LogService logService;

	@Autowired
	private MammaBeoordelingRepository beoordelingRepository;

	@Override
	public MammaLezingRapportageDto getLezingRapportage(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		MammaLezingRapportageDto lezingDto = new MammaLezingRapportageDto();
		lezingDto.setAantalEersteLezingen(getEersteLezingenCount(instellingGebruiker, date, termijn));
		lezingDto.setAantalTweedeLezingen(getTweedeLezingenCount(instellingGebruiker, date, termijn));
		lezingDto.setTotaalAantalLezingen(lezingDto.getAantalEersteLezingen() + lezingDto.getAantalTweedeLezingen());
		lezingDto.setAantalDiscrepantieLezingen(getDiscrepantieLezingenCount(instellingGebruiker, date, termijn));
		if (Termijn.KALENDERJAAR.equals(termijn))
		{
			lezingDto.setPercentageVerwijzingenEersteRonde(
				getPercentageRondeVerwijzend(eersteOf2deLezerOngunstigeUitslagEersteRondesCount(instellingGebruiker, date, termijn), lezingDto.getTotaalAantalLezingen()));
			lezingDto.setPercentageVerwijzingenMeerdereRondes(
				getPercentageRondeVerwijzend(eersteOf2deLezerOngunstigeUitslagVervolgRondesCount(instellingGebruiker, date, termijn), lezingDto.getTotaalAantalLezingen()));
		}
		return lezingDto;
	}

	private long getEersteLezingenCount(InstellingGebruiker radioloog, LocalDate datum, Termijn termijn)
	{
		return beoordelingRepository.count(isGedaanBinnenTermijnDoor(radioloog, datum, termijn).with(MammaBeoordeling_.eersteLezing));
	}

	private long getTweedeLezingenCount(InstellingGebruiker radioloog, LocalDate datum, Termijn termijn)
	{
		return beoordelingRepository.count(isGedaanBinnenTermijnDoor(radioloog, datum, termijn).with(MammaBeoordeling_.tweedeLezing));
	}

	private long getDiscrepantieLezingenCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		return beoordelingRepository.count(heeftEersteOfTweedeLezingGedaanBinnenTermijn(instellingGebruiker, date, termijn).and(heeftTotDiscrepantieGeleid()));
	}

	private long eersteOf2deLezerOngunstigeUitslagEersteRondesCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		return beoordelingRepository.count(heeftEersteOfTweedeLezingGedaanBinnenTermijn(instellingGebruiker, date, termijn)
			.and(heeftStatus(UITSLAG_ONGUNSTIG))
			.and(heeftPreciesEenRonde().with(r -> dossierJoin(r)))
		);
	}

	private long eersteOf2deLezerOngunstigeUitslagVervolgRondesCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		return beoordelingRepository.count(heeftEersteOfTweedeLezingGedaanBinnenTermijn(instellingGebruiker, date, termijn)
			.and(heeftStatus(UITSLAG_ONGUNSTIG))
			.and(not(heeftPreciesEenRonde().with(r -> dossierJoin(r)))));
	}

	private static Join<?, MammaDossier> dossierJoin(From<?, ? extends MammaBeoordeling> beoordelingRoot)
	{
		var onderzoekJoin = join(beoordelingRoot, MammaBeoordeling_.onderzoek);
		var afspraakJoin = join(onderzoekJoin, MammaOnderzoek_.afspraak);
		var uitnodigingJoin = join(afspraakJoin, MammaAfspraak_.uitnodiging);
		var screeningRondeJoin = join(uitnodigingJoin, MammaUitnodiging_.screeningRonde);
		return join(screeningRondeJoin, MammaScreeningRonde_.dossier);
	}

	private String getPercentageRondeVerwijzend(long nRondeUitslagen, long totaalBeoordeling1Of2Lezing)
	{
		String uitslagString = "Geen beoordelingen";
		if (totaalBeoordeling1Of2Lezing != 0L)
		{
			uitslagString = PercentageUtil.getPercentageVanGeheel(nRondeUitslagen, totaalBeoordeling1Of2Lezing);
		}
		return uitslagString;
	}

	@Override
	public void logPopupPreBirads(Client client, InstellingGebruiker gebruiker, MammaLezing lezing, MammaBIRADSWaarde prePopupBiradsWaardeLinks,
		MammaBIRADSWaarde prePopupBiradsWaardeRechts)
	{
		String melding = String.format("Lezing BI-RADS waardes voor popup: BI-RADS: %s%s. BI-RADS waardes na popup: %s%s",
			MammaScreeningRondeUtil.bepaalNaamBiradsWaarde(MammaZijde.RECHTER_BORST, prePopupBiradsWaardeRechts),
			MammaScreeningRondeUtil.bepaalNaamBiradsWaarde(MammaZijde.LINKER_BORST, prePopupBiradsWaardeLinks),
			MammaScreeningRondeUtil.bepaalNaamBiradsWaarde(MammaZijde.RECHTER_BORST, lezing.getBiradsRechts()),
			MammaScreeningRondeUtil.bepaalNaamBiradsWaarde(MammaZijde.LINKER_BORST, lezing.getBiradsLinks()));
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_BEOORDELING_SIGNALERING_GETOOND, gebruiker, client, melding, Bevolkingsonderzoek.MAMMA);
	}
}
