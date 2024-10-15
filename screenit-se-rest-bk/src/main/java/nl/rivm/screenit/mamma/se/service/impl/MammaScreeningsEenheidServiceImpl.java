package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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
import java.util.Collections;
import java.util.List;

import lombok.RequiredArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.MammaScreeningsEenheidService;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid_;
import nl.rivm.screenit.repository.mamma.MammaScreeningsEenheidRepository;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.mamma.se.service.impl.ConfiguratieServiceImpl.SE_DAGLIJST_OPHALEN_VOOR_DAGEN_DEFAULT;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.heeftCode;
import static nl.rivm.screenit.specification.mamma.MammaScreeningsEenheidSpecification.isActief;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
@RequiredArgsConstructor
public class MammaScreeningsEenheidServiceImpl implements MammaScreeningsEenheidService
{
	private final MammaAfspraakService mammaAfspraakService;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	private final MammaScreeningsEenheidRepository screeningsEenheidRepository;

	@Override
	public MammaScreeningsEenheid getActieveScreeningsEenheidByCode(String seCode)
	{
		return screeningsEenheidRepository.findOne(heeftCode(seCode).and(isActief())).orElse(null);
	}

	@Override
	public String getSeCodeMetIpAdres(String ipAdres)
	{
		return screeningsEenheidRepository.findWith(
			MammaScreeningsEenheidSpecification.heeftIpAdres(ipAdres).and(isActief()),
			String.class,
			q -> q.projection((cb, r) -> r.get(MammaScreeningsEenheid_.code))
				.one().orElse(null));
	}

	@Override
	public boolean magSeDaglijstInzienVanDatum(String seCode, LocalDate datum)
	{

		var laatstOpTeHalenDatum = getLaatstOpTeHalenDatum();
		var vandaag = currentDateSupplier.getLocalDate();

		if (!datum.isBefore(vandaag) && !datum.isAfter(laatstOpTeHalenDatum))
		{
			return true;
		}
		if (datum.isAfter(laatstOpTeHalenDatum))
		{
			return false;
		}

		var vroegstOpTeHalenDatum = getVroegstOpTeHalenDatum(seCode);
		return !datum.isBefore(vroegstOpTeHalenDatum);
	}

	private LocalDate getVroegstOpTeHalenDatum(String seCode)
	{
		var datumVanOudsteNietAfgeslotenOnderzoek = mammaAfspraakService.getDatumVanOudsteNietAfgeslotenOnderzoek(seCode);
		var vandaag = currentDateSupplier.getLocalDate();
		if (datumVanOudsteNietAfgeslotenOnderzoek != null)
		{
			return Collections.min(List.of(datumVanOudsteNietAfgeslotenOnderzoek, vandaag));
		}
		return vandaag;
	}

	private LocalDate getLaatstOpTeHalenDatum()
	{
		var seOphalenAantalDagen = preferenceService.getInteger(PreferenceKey.MAMMA_SE_DAGLIJST_OPHALEN_DAGEN.name(), SE_DAGLIJST_OPHALEN_VOOR_DAGEN_DEFAULT);
		var vandaag = currentDateSupplier.getLocalDate();
		return vandaag.plusDays(seOphalenAantalDagen);
	}
}
