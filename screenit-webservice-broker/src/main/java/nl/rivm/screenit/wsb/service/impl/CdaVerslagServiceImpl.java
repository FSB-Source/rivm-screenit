package nl.rivm.screenit.wsb.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-webservice-broker
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.CdaVerslagDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.berichten.cda.OntvangenCdaBericht;
import nl.rivm.screenit.model.berichten.cda.PdBerichtResponseCode;
import nl.rivm.screenit.model.berichten.cda.PdBerichtResponseResult;
import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.berichten.enums.BerichtType;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.BerichtOntvangenLogEvent;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.wsb.service.CdaVerslagService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CdaVerslagServiceImpl implements CdaVerslagService
{

	@Autowired
	private CdaVerslagDao cdaVerslagDao;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private LogService logService;

	@Autowired
	private ClientService clientService;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public PdBerichtResponseResult valideerBericht(OntvangenCdaBericht ontvangenCdaBericht, String bsn, String remoteAddr, String orgInfo)
	{
		PdBerichtResponseResult result = new PdBerichtResponseResult(PdBerichtResponseCode.OK);
		String berichtId = ontvangenCdaBericht.getBerichtId();
		BerichtType berichtType = ontvangenCdaBericht.getBerichtType();
		Bevolkingsonderzoek bvo = berichtType.getBevolkingsonderzoek();
		if (cdaVerslagDao.isBerichtReedsVerwerkt(berichtId))
		{
			PdBerichtResponseCode code = PdBerichtResponseCode.REEDS_CORRECT_VERWERKT;
			String codeOmschrijving = String.format(code.toString(), berichtId);

			meldFout(ontvangenCdaBericht, remoteAddr, orgInfo, result, bvo, code, codeOmschrijving, berichtType.getLbBerichtZelfdeId(), BerichtStatus.WAARSCHUWING);
		}
		else
		{
			String setId = ontvangenCdaBericht.getSetId();
			Long versie = ontvangenCdaBericht.getVersie();
			if (cdaVerslagDao.isBerichtReedsOntvangen(setId, versie))
			{
				PdBerichtResponseCode code = PdBerichtResponseCode.ONGELDIGE_VERSIE;
				String codeOmschrijving = String.format(code.toString(), setId, versie);

				meldFout(ontvangenCdaBericht, remoteAddr, orgInfo, result, bvo, code, codeOmschrijving, berichtType.getLbBerichtZelfdeSetIdEnVersie(), BerichtStatus.WAARSCHUWING);
			}
			else
			{
				Client clientByBsn = clientService.getClientByBsn(bsn);
				if (clientByBsn == null)
				{
					PdBerichtResponseCode code = PdBerichtResponseCode.CLIENT_UNK;
					String codeOmschrijving = String.format(code.toString(), ontvangenCdaBericht.getBerichtId(), bsn);

					meldFout(ontvangenCdaBericht, remoteAddr, orgInfo, result, bvo, code, codeOmschrijving, berichtType.getLbOnbekendeBsn(), BerichtStatus.FOUT);
				}
				else if (bvo == Bevolkingsonderzoek.COLON && clientService.heeftClientIntakeConclusieMetBezwaar(bsn))
				{
					PdBerichtResponseCode code = PdBerichtResponseCode.CLIENT_BEZWAAR;
					String codeOmschrijving = String.format(code.toString(), bsn);

					meldFout(ontvangenCdaBericht, remoteAddr, orgInfo, result, bvo, code, codeOmschrijving, berichtType.getLbOndanksBezwaar(), BerichtStatus.FOUT);
				}
			}
		}
		hibernateService.saveOrUpdate(ontvangenCdaBericht);
		return result;
	}

	private void meldFout(OntvangenCdaBericht ontvangenCdaBericht, String remoteAddr, String orgInfo, PdBerichtResponseResult result, Bevolkingsonderzoek bvo,
		PdBerichtResponseCode code, String codeOmschrijving, LogGebeurtenis logGebeurtenis, BerichtStatus berichtStatus)
	{
		hibernateService.saveOrUpdate(ontvangenCdaBericht);
		result.setCode(code);
		result.setOmschrijving(codeOmschrijving);

		if (logGebeurtenis != null)
		{
			String logMelding = codeOmschrijving + " (van " + remoteAddr + ")" + orgInfo + "."
				+ (berichtStatus == BerichtStatus.WAARSCHUWING ? " Wordt niet nog een keer verwerkt." : "");
			BerichtOntvangenLogEvent logEvent = new BerichtOntvangenLogEvent();
			logEvent.setBericht(ontvangenCdaBericht);
			logEvent.setMelding(logMelding);

			logService.logGebeurtenis(logGebeurtenis, logEvent, bvo);
		}

		ontvangenCdaBericht.setStatus(berichtStatus);
	}
}
