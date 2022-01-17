package nl.rivm.screenit.service.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Iterator;

import nl.rivm.screenit.dao.colon.HuisartsBerichtDao;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.colon.ColonHuisartsBericht;
import nl.rivm.screenit.model.colon.ColonHuisartsBerichtStatus;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.HuisartsBerichtType;
import nl.rivm.screenit.service.colon.ColonEdiService;
import nl.rivm.screenit.service.colon.ColonHuisartsBerichtService;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class ColonHuisartsBerichtServiceImpl implements ColonHuisartsBerichtService
{

	@Autowired(required = false)
	private HuisartsBerichtDao berichtDao;

	@Autowired
	private SimplePreferenceService simplePreferenceService;

	@Autowired
	private ColonEdiService ediService;

	@Autowired
	@Qualifier(value = "smtpAuthUsername")
	private String username;

	@Autowired
	@Qualifier(value = "smtpAuthPassword")
	private String password;

	@Autowired
	@Qualifier(value = "mailRelayIp")
	private String host;

	@Autowired
	@Qualifier(value = "mailRelayPort")
	private Integer port;

	@Autowired
	@Qualifier(value = "smtpOverSsl")
	private Boolean ssl;

	@Autowired
	@Qualifier("afzendEmailadres")
	private String afzendEmailadres;

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public long countBerichten(ColonHuisartsBericht zoekObject, Instelling regioObject)
	{
		return berichtDao.countBerichten(zoekObject, regioObject);
	}

	@Override
	@Transactional(propagation = Propagation.SUPPORTS)
	public Iterator<? extends ColonHuisartsBericht> searchBerichten(ColonHuisartsBericht zoekObject, Instelling regioObject, String sortProperty,
		boolean ascending, int first, int count, boolean werklijst)
	{
		return berichtDao.searchBerichten(zoekObject, regioObject, sortProperty, ascending, first, count, werklijst);
	}

	@Override
	public void verstuurColonHuisartsBericht(Client client, ColonScreeningRonde colonScreeningRonde, HuisartsBerichtType berichtType,
		MailMergeContext context)
	{
		verstuurColonHuisartsBericht(client, colonScreeningRonde, berichtType, context, false);
	}

	@Override
	public void verstuurColonHuisartsBericht(Client client, ColonScreeningRonde colonScreeningRonde, HuisartsBerichtType berichtType,
		MailMergeContext context, boolean opnieuwVerzonden)
	{
		if (colonScreeningRonde == null
			|| BezwaarUtil.isBezwaarActiefVoor(client, BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS, Bevolkingsonderzoek.COLON))
		{
			return;
		}
		EnovationHuisarts huisarts = colonScreeningRonde.getColonHuisarts();
		if (huisarts != null)
		{
			ColonHuisartsBericht huisartsBericht = ediService.maakHuisartsBericht(berichtType, ColonHuisartsBerichtStatus.CONTROLE_NIET_NODIG, client,
				huisarts, context, opnieuwVerzonden);
			ediService.verstuurMedVry(huisartsBericht);
		}
	}
}
