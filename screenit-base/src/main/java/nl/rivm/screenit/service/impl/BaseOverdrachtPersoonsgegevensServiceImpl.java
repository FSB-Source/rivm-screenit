package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BaseOverdrachtPersoonsgegevensService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class BaseOverdrachtPersoonsgegevensServiceImpl implements BaseOverdrachtPersoonsgegevensService
{
    @Autowired
    private HibernateService hibernateService;

    @Autowired
    private ICurrentDateSupplier currentDateSupplier;

    @Autowired
    private BaseBriefService briefService;

    @Override
    public void maakOverdrachtVerzoek(Client client)
    {
        AlgemeneBrief brief = briefService.maakAlgemeneBrief(client, BriefType.CLIENT_INZAGE_PERSOONSGEGEVENS_AANVRAAG);
        OverdrachtPersoonsgegevens overdracht = new OverdrachtPersoonsgegevens();
        overdracht.setClient(client);
        overdracht.setVerstuurdeAanvraagbrief(brief);
        overdracht.setStatus(AanvraagBriefStatus.BRIEF);
        overdracht.setStatusDatum(currentDateSupplier.getDate());
        overdracht.setBkGegevens(false);
        overdracht.setBkBeelden(false);
        overdracht.setBmhkGegevens(false);
        overdracht.setDkGegevens(false);
        hibernateService.saveOrUpdate(overdracht);

    }

}
