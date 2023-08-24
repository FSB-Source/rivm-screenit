package nl.rivm.screenit.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Comparator;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.DigitaalClientBericht;
import nl.rivm.screenit.model.enums.DigitaalBerichtType;
import nl.rivm.screenit.model.mamma.MammaDigitaalClientBericht;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.repository.mamma.MammaDigitaalClientBerichtRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaDigitaalClientBerichtService;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@AllArgsConstructor
@Transactional(propagation = Propagation.REQUIRED)
public class MammaDigitaalClientBerichtServiceImpl implements MammaDigitaalClientBerichtService
{
	private MammaDigitaalClientBerichtRepository digitaalClientBerichtRepository;

	private ICurrentDateSupplier currentDateSupplier;

	private HibernateService hibernateService;

	@Override
	public void saveOrUpdate(MammaDigitaalClientBericht digitaalBericht)
	{
		var clientBerichtNieuw = digitaalClientBerichtRepository.save(digitaalBericht);
		updateAfspraakEnRondeBijNieuwBericht(clientBerichtNieuw);
	}

	private void updateAfspraakEnRondeBijNieuwBericht(MammaDigitaalClientBericht digitaalBericht)
	{
		var ronde = digitaalBericht.getScreeningRonde();
		ronde.getBerichten().add(digitaalBericht);
		hibernateService.saveOrUpdate(ronde);
	}

	@Override
	public boolean digitaalClientBerichtMagOpnieuwVerzondenWorden(MammaDigitaalClientBericht clientBericht)
	{
		var rondeUitMail = clientBericht.getScreeningRonde();
		var laatsteScreeningRonde = rondeUitMail.getDossier().getLaatsteScreeningRonde();
		var laatsteAfspraakLaatsteRonde = MammaScreeningRondeUtil.getLaatsteAfspraak(laatsteScreeningRonde);

		var berichtIsMail = DigitaalBerichtType.EMAIL == clientBericht.getDigitaalBerichtTemplateType().getBerichtType();
		var afspraakIsNogNietGeweest = laatsteAfspraakLaatsteRonde != null && MammaAfspraakStatus.GEPLAND == laatsteAfspraakLaatsteRonde.getStatus()
			&& currentDateSupplier.getLocalDateTime().isBefore(DateUtil.toLocalDateTime(laatsteAfspraakLaatsteRonde.getVanaf()));

		return berichtIsMail && !clientBericht.getIsHerzonden() && afspraakIsNogNietGeweest && berichtIsLaatstVerzondenOorspronkelijkeBerichtVanType(clientBericht);
	}

	private boolean berichtIsLaatstVerzondenOorspronkelijkeBerichtVanType(MammaDigitaalClientBericht clientBericht)
	{
		var berichtType = clientBericht.getDigitaalBerichtTemplateType();

		var laatsteOorspronkelijkeBerichtLaatsteRonde = clientBericht.getScreeningRonde().getDossier().getLaatsteScreeningRonde().getBerichten().stream()
			.filter(bericht -> bericht.getDigitaalBerichtTemplateType().equals(berichtType))
			.filter(bericht -> !bericht.getIsHerzonden())
			.max(Comparator.comparing(DigitaalClientBericht::getCreatieMoment))
			.orElse(null);

		return clientBericht.equals(laatsteOorspronkelijkeBerichtLaatsteRonde);
	}
}
