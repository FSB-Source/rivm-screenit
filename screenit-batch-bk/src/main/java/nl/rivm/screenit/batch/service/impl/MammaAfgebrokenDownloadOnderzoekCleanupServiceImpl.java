package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.batch.service.MammaAfgebrokenDownloadOnderzoekCleanupService;
import nl.rivm.screenit.model.enums.BestandStatus;
import nl.rivm.screenit.model.mamma.MammaDownloadOnderzoek;
import nl.rivm.screenit.repository.mamma.MammaDownloadOnderzoekenVerzoekRepository;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
@RequiredArgsConstructor
public class MammaAfgebrokenDownloadOnderzoekCleanupServiceImpl implements MammaAfgebrokenDownloadOnderzoekCleanupService
{
	private final MammaDownloadOnderzoekenVerzoekRepository downloadOnderzoekenVerzoekRepository;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void checkAfgebrokenDownloadOnderzoekenVerzoekEnRuimOp()
	{
		var onderbrokenDownloadOnderzoekenVerzoeken = downloadOnderzoekenVerzoekRepository.getMammaDownloadOnderzoekenVerzoekByStatus(BestandStatus.BEZIG_MET_VERWERKEN);

		if (!onderbrokenDownloadOnderzoekenVerzoeken.isEmpty())
		{
			LOG.info("{} onderbroken MammaDownloadOnderzoekenVerzoek(en) gevonden.", onderbrokenDownloadOnderzoekenVerzoeken.size());
			onderbrokenDownloadOnderzoekenVerzoeken.forEach(verzoek ->
			{
				LOG.info("MammaDownloadOnderzoekenVerzoek met id: '{}' op status crash gezet.", verzoek.getId());
				verzoek.setStatus(BestandStatus.CRASH);

				verzoek.getOnderzoeken().forEach(this::fixDownloadOnderzoek);
			});

		}
	}

	private void fixDownloadOnderzoek(MammaDownloadOnderzoek downloadOnderzoek)
	{

		if (downloadOnderzoek.getStatus() == BestandStatus.BEZIG_MET_VERWERKEN)
		{
			LOG.info("MammaDownloadOnderzoek met id: '{}' op status crash gezet.", downloadOnderzoek.getId());
			downloadOnderzoek.setStatus(BestandStatus.CRASH);
			downloadOnderzoek.setStatusMelding("Afgebroken");
		}
	}
}
