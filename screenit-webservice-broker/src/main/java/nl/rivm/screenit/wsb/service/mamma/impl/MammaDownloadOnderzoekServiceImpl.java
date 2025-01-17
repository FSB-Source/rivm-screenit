package nl.rivm.screenit.wsb.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
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

import java.util.Optional;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek_;
import nl.rivm.screenit.repository.mamma.MammaDownloadOnderzoekRepository;
import nl.rivm.screenit.specification.mamma.MammaDownloadOnderzoekSpecification;
import nl.rivm.screenit.specification.mamma.MammaDownloadOnderzoekenVerzoekSpecification;
import nl.rivm.screenit.wsb.service.mamma.MammaDownloadOnderzoekService;

import org.springframework.stereotype.Service;

import static nl.rivm.screenit.model.enums.BestandStatus.BEZIG_MET_VERWERKEN;
import static nl.rivm.screenit.specification.mamma.MammaDownloadOnderzoekSpecification.heeftUitnodigingsNummer;

@Service
@AllArgsConstructor
public class MammaDownloadOnderzoekServiceImpl implements MammaDownloadOnderzoekService
{
	private final MammaDownloadOnderzoekRepository mammaDownloadOnderzoekRepository;

	@Override
	public Optional<MammaDownloadOnderzoek> findDownloadOnderzoekInVerwerking(long uitnodigingsNummer)
	{
		return mammaDownloadOnderzoekRepository.findOne(
			heeftUitnodigingsNummer(uitnodigingsNummer)
				.and(MammaDownloadOnderzoekSpecification.heeftStatus(BEZIG_MET_VERWERKEN))
				.and(MammaDownloadOnderzoekenVerzoekSpecification.heeftStatus(BEZIG_MET_VERWERKEN).with(MammaDownloadOnderzoek_.verzoek)));
	}
}
