package nl.rivm.screenit.dao.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.UploadDocument;
import nl.topicuszorg.documentupload.dao.UploadDocumentDao;
import nl.topicuszorg.documentupload.model.IUploadDocument;
import nl.topicuszorg.documentupload.model.IUploadDocumentParent;
import nl.topicuszorg.hibernate.spring.dao.impl.AbstractAutowiredDao;

import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation = Propagation.REQUIRED)
public class UploadDocumentDaoImpl extends AbstractAutowiredDao implements UploadDocumentDao
{

	@Override
	public void saveOrUpdate(IUploadDocument iUploadDocument)
	{
		getSession().saveOrUpdate(iUploadDocument);
	}

	@Override
	public void saveOrUpdate(IUploadDocumentParent iUploadDocumentParent)
	{
		getSession().saveOrUpdate(iUploadDocumentParent);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void delete(IUploadDocument iUploadDocument)
	{
		getSession().delete(iUploadDocument);
	}
}
