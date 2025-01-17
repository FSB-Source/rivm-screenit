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
import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.form.FilterBvoFormPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.batch.BvoZoekCriteria;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.MergeFieldTestType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.afmeldbrief.ColonAfmeldDocumentCreator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.wicketstuff.shiro.ShiroConstraint;

import com.aspose.words.Document;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_BEHEER_DOCUMENTENTEMPLATES,
	bevolkingsonderzoekScopes = {
		Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA },
	organisatieTypeScopes = { OrganisatieType.SCREENINGSORGANISATIE, OrganisatieType.RIVM })
public class DocumentTemplateTestenPage extends BaseDocumentTemplateTestenPage
{
	private IModel<BvoZoekCriteria> zoekFilter;

	private List<BriefType> visibleBriefTypes = null;

	@Override
	protected List<MergeFieldTestType> getMergeTypes()
	{
		var values = new ArrayList<>(Arrays.asList(MergeFieldTestType.values()));
		values.remove(MergeFieldTestType.ZORGINSTELLING);
		values.remove(MergeFieldTestType.ZORGVERLENER);
		return values;
	}

	@Override
	protected void addAdditionalFormComponents(Form<Void> form)
	{
		zoekFilter = Model.of(new BvoZoekCriteria());
		zoekFilter.getObject().setBevolkingsonderzoeken(ScreenitSession.get().getOnderzoeken());
		form.add(new FilterBvoFormPanel<BvoZoekCriteria>("bvoFilter", zoekFilter, true, true)
		{

			@Override
			protected void doFilter(IModel<BvoZoekCriteria> filterModel, AjaxRequestTarget target)
			{
				visibleBriefTypes = null;
				target.add(form);
			}
		});
	}

	@Override
	protected void mergeFieldChanged(AjaxRequestTarget target)
	{
		visibleBriefTypes = null;
	}

	@Override
	protected List<BriefType> getVisibleBriefTypes()
	{
		if (visibleBriefTypes == null)
		{
			var bevolkingsonderzoeken = zoekFilter.getObject().getBevolkingsonderzoeken();
			visibleBriefTypes = new ArrayList<>(BriefType.getBriefTypes(Boolean.TRUE.equals(zoekFilter.getObject().getExactMatch()),
				bevolkingsonderzoeken.toArray(new Bevolkingsonderzoek[bevolkingsonderzoeken.size()])));
			visibleBriefTypes.remove(BriefType.CLIENT_BEZWAAR_AANVRAAG);
			visibleBriefTypes.remove(BriefType.CLIENT_BEZWAAR_BEVESTIGING);
			visibleBriefTypes.remove(BriefType.CLIENT_BEZWAAR_HANDTEKENING);

			Comparator<BriefType> com = (o1, o2) ->
			{
				var string1 = Bevolkingsonderzoek.getAfkortingen(o1.getOnderzoeken()) + " - " + o1.getCodeEnNaam();
				var string2 = Bevolkingsonderzoek.getAfkortingen(o2.getOnderzoeken()) + " - " + o2.getCodeEnNaam();
				return string1.compareTo(string2);
			};
			visibleBriefTypes.sort(com);
		}
		if (mergeFieldModel.getObject() != null)
		{
			visibleBriefTypes = zichtbareBriefTypesMetMergeField(visibleBriefTypes, mergeFieldModel.getObject());
		}
		return visibleBriefTypes;
	}

	@Override
	protected List<Bevolkingsonderzoek> getBevolkingsonderzoeken()
	{
		return zoekFilter.getObject().getBevolkingsonderzoeken();
	}

	@Override
	protected Document proccesDocument(MailMergeContext context, File briefTemplate) throws Exception
	{
		if (context.getBrief() != null && (BriefType.COLON_AFMELDING_AANVRAAG == context.getBrief().getBriefType() || BriefType.COLON_AFMELDING_HANDTEKENING == context.getBrief()
			.getBriefType()))
		{
			return asposeService.processDocumentWithCreator(context, briefTemplate, new ColonAfmeldDocumentCreator(List.of(2025, 2026)), true);
		}
		return asposeService.processDocument(briefTemplate, context);
	}
}
