package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.math.MathContext;
import java.time.LocalDate;

import nl.rivm.screenit.dto.mamma.MammaLezingRapportageDto;
import nl.rivm.screenit.main.dao.mamma.MammaLezingRapportageDao;
import nl.rivm.screenit.main.service.mamma.MammaLezingService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Termijn;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaBIRADSWaarde;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.util.PercentageUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class MammaLezingServiceImpl implements MammaLezingService
{

	@Autowired
	private LogService logService;

	@Autowired
	private MammaLezingRapportageDao lezingDao;

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

	private long getEersteLezingenCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		return lezingDao.eersteLezingenCount(instellingGebruiker, date, termijn);
	}

	private long getTweedeLezingenCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		return lezingDao.tweedeLezingenCount(instellingGebruiker, date, termijn);
	}

	private long eersteOf2deLezerOngunstigeUitslagVervolgRondesCount(
		InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		return lezingDao.eersteOf2deLezerOngunstigeUitslagVervolgRondesCount(instellingGebruiker, date, termijn);
	}

	private long eersteOf2deLezerOngunstigeUitslagEersteRondesCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		return lezingDao.eersteOf2deLezerOngunstigeUitslagEersteRondesCount(instellingGebruiker, date, termijn);
	}

	private long getDiscrepantieLezingenCount(InstellingGebruiker instellingGebruiker, LocalDate date, Termijn termijn)
	{
		return lezingDao.discrepantieLezingenCount(instellingGebruiker, date, termijn);
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
