package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.util.EnumStringUtil;
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

	public DocumentTemplateTestenPage()
	{
	}

	@Override
	protected List<MergeFieldTestType> getMergeTypes()
	{
		List<MergeFieldTestType> values = new ArrayList<>(Arrays.asList(MergeFieldTestType.values()));
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
	protected List<BriefType> getVisibleBriefTypes()
	{
		if (visibleBriefTypes == null)
		{
			List<Bevolkingsonderzoek> bevolkingsonderzoeken = zoekFilter.getObject().getBevolkingsonderzoeken();
			visibleBriefTypes = new ArrayList<>(BriefType.getBriefTypes(Boolean.TRUE.equals(zoekFilter.getObject().getExactMatch()),
				bevolkingsonderzoeken.toArray(new Bevolkingsonderzoek[bevolkingsonderzoeken.size()])));
			visibleBriefTypes.remove(BriefType.CLIENT_BEZWAAR_AANVRAAG);
			visibleBriefTypes.remove(BriefType.CLIENT_BEZWAAR_BEVESTIGING);
			visibleBriefTypes.remove(BriefType.CLIENT_BEZWAAR_HANDTEKENING);
			Comparator<BriefType> com = new Comparator<BriefType>()
			{
				@Override
				public int compare(BriefType o1, BriefType o2)
				{
					String key1 = EnumStringUtil.getPropertyString(o1);
					String key2 = EnumStringUtil.getPropertyString(o2);
					String string1 = Bevolkingsonderzoek.getAfkortingen(o1.getOnderzoeken()) + " - " + getString(key1, null);
					String string2 = Bevolkingsonderzoek.getAfkortingen(o2.getOnderzoeken()) + " - " + getString(key2, null);
					return string1.compareTo(string2);
				}
			};
			Collections.sort(visibleBriefTypes, com);
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
		return asposeService.processDocument(briefTemplate, context);
	}
}
