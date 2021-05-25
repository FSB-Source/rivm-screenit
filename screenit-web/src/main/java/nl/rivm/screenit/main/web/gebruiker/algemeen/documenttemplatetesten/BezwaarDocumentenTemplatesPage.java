package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.document.bezwaar.BezwaarDocumentCreatorOneDatasetCoupleTables;
import nl.rivm.screenit.main.web.component.bezwaar.edit.BezwaarEditPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.MergeFieldTestType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.BezwaarService;

import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wicketstuff.shiro.ShiroConstraint;

import com.aspose.words.Document;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_BEHEER_DOCUMENTENTEMPLATES,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class BezwaarDocumentenTemplatesPage extends BaseDocumentTemplateTestenPage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(BezwaarDocumentenTemplatesPage.class);

	@SpringBean
	private BezwaarService bezwaarService;

	private List<BezwaarGroupViewWrapper> wrappers;

	public BezwaarDocumentenTemplatesPage()
	{
	}

	@Override
	protected List<BriefType> getVisibleBriefTypes()
	{
		List<BriefType> briefTypes = new ArrayList<>();
		briefTypes.add(BriefType.CLIENT_BEZWAAR_AANVRAAG);
		briefTypes.add(BriefType.CLIENT_BEZWAAR_BEVESTIGING);
		briefTypes.add(BriefType.CLIENT_BEZWAAR_HANDTEKENING);
		return briefTypes;
	}

	@Override
	protected void addAdditionalFormComponents(Form<Void> form)
	{
		BezwaarMoment moment = new BezwaarMoment();
		wrappers = bezwaarService.getEditBezwaarGroupViewWrappers(null, moment);
		form.add(new BezwaarEditPanel("bezwaarAanpassenPanel", wrappers, true));
	}

	@Override
	protected List<MergeFieldTestType> getMergeTypes()
	{
		List<MergeFieldTestType> fieldTestType = new ArrayList<>();
		fieldTestType.add(MergeFieldTestType.CLIENT);
		fieldTestType.add(MergeFieldTestType.OVERIGE);
		return fieldTestType;
	}

	@Override
	protected List<Bevolkingsonderzoek> getBevolkingsonderzoeken()
	{
		return Arrays.asList(new Bevolkingsonderzoek[] { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA });
	}

	@Override
	protected Document proccesDocument(MailMergeContext context, File briefTemplate) throws Exception
	{
		BezwaarDocumentCreatorOneDatasetCoupleTables creator = new BezwaarDocumentCreatorOneDatasetCoupleTables(wrappers);
		return asposeService.processDocumentWithCreator(context, briefTemplate, creator, true);
	}
}
