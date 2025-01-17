package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

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

import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

import com.aspose.words.Document;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_BEHEER_DOCUMENTENTEMPLATES,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
@Slf4j
public class BezwaarDocumentenTemplatesPage extends BaseDocumentTemplateTestenPage
{
	@SpringBean
	private BezwaarService bezwaarService;

	private List<BezwaarGroupViewWrapper> wrappers;

	@Override
	protected List<BriefType> getVisibleBriefTypes()
	{
		List<BriefType> briefTypes = BriefType.CLIENT_BEZWAAR_BRIEVEN;
		if (mergeFieldModel.getObject() != null)
		{
			briefTypes = zichtbareBriefTypesMetMergeField(briefTypes, mergeFieldModel.getObject());
		}
		return briefTypes;
	}

	@Override
	protected void addAdditionalFormComponents(Form<Void> form)
	{
		var moment = new BezwaarMoment();
		wrappers = bezwaarService.getEditBezwaarGroupViewWrappers(null, moment);
		form.add(new BezwaarEditPanel("bezwaarAanpassenPanel", wrappers, true));

		var zonderHandtekeningRadioChoice = new RadioChoice<>("zonderHandtekening", zonderHandtekeningModel,
			new ListModel<>(Arrays.asList(Boolean.FALSE, Boolean.TRUE)),
			new ChoiceRenderer<>()
			{
				@Override
				public Object getDisplayValue(Boolean object)
				{
					return Boolean.TRUE.equals(object) ? "Zonder handtekening" : "Met handtekening";
				}
			}
		);
		zonderHandtekeningRadioChoice.setPrefix("<label class=\"radio\">");
		zonderHandtekeningRadioChoice.setSuffix("</label>");
		zonderHandtekeningRadioChoice.setOutputMarkupId(true);
		form.add(zonderHandtekeningRadioChoice);
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
		return Arrays.asList(Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA);
	}

	@Override
	protected Document proccesDocument(MailMergeContext context, File briefTemplate) throws Exception
	{
		var creator = new BezwaarDocumentCreatorOneDatasetCoupleTables(wrappers, getSelectedType());
		return asposeService.processDocumentWithCreator(context, briefTemplate, creator, true);
	}
}
