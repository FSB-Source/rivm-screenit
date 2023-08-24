package nl.rivm.screenit.service.impl;

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

import nl.rivm.screenit.model.Account;
import nl.rivm.screenit.model.enums.GebeurtenisBron;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.envers.ScreenitRevisionEntity;
import nl.rivm.screenit.service.BaseDossierAuditService;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import org.hibernate.envers.query.AuditEntity;
import org.hibernate.envers.query.AuditQuery;
import org.hibernate.envers.query.criteria.AuditCriterion;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class BaseDossierAuditServiceImpl implements BaseDossierAuditService
{

    @Autowired
    private HibernateService hibernateService;

    @Override
    public GebeurtenisBron getGebeurtenisBron(ScreenitRevisionEntity revisionEntity, GebeurtenisBron defaultIfNull)
    {
        GebeurtenisBron bron;
        if (revisionEntity == null)
        {
            bron = defaultIfNull;
        }
        else if (revisionEntity.getClient() != null)
        {
            bron = GebeurtenisBron.CLIENT;
        }
        else if (revisionEntity.getGebruiker() != null || revisionEntity.getInstellingGebruiker() != null)
        {
            bron = GebeurtenisBron.MEDEWERKER;
        }
        else
        {
            bron = GebeurtenisBron.AUTOMATISCH;
        }
        return bron;
    }

    @Override
    public GebeurtenisBron getGebeurtenisBron(ScreenitRevisionEntity revisionEntity)
    {
        return getGebeurtenisBron(revisionEntity, null);
    }

    @Override
    public ScreenitRevisionEntity getLastRevision(HibernateObject entity, AuditCriterion extraCriteria)
    {
        return getLastRevision(entity, extraCriteria, null);
    }

    @Override
    public ScreenitRevisionEntity getLastRevision(HibernateObject entity, AuditCriterion extraCriteria, Class<? extends Account> accountType)
    {
        AuditQuery query = EntityAuditUtil.createQuery(entity, hibernateService.getHibernateSession());

        query.addProjection(AuditEntity.revisionNumber().max());
        if (Client.class.equals(accountType))
        {
            query.add(AuditEntity.revisionProperty("client").isNotNull());
        }
        if (extraCriteria != null)
        {
            query.add(extraCriteria);
        }
        Number lastRevision = (Number) query.getSingleResult();
        ScreenitRevisionEntity revisionEntity = null;
        if (lastRevision != null)
        {
            revisionEntity = hibernateService.get(ScreenitRevisionEntity.class, lastRevision);
        }
        return revisionEntity;
    }

    @Override
    public Date getLastRevisionDate(HibernateObject entity, AuditCriterion extraCriteria, Class<? extends Account> accountType)
    {
        Date actionDate = null;
        ScreenitRevisionEntity revisionEntity = getLastRevision(entity, extraCriteria, accountType);

        if (revisionEntity != null && revisionEntity.getTimestamp() > 0)
        {
            actionDate = new Date(revisionEntity.getTimestamp());
        }
        return actionDate;
    }

    @Override
    public Date getDateForRevisionEntity(ScreenitRevisionEntity revisionEntity)
    {
        Date actionDate = null;
        if (revisionEntity != null && revisionEntity.getTimestamp() > 0)
        {
            actionDate = new Date(revisionEntity.getTimestamp());
        }
        return actionDate;
    }

}
