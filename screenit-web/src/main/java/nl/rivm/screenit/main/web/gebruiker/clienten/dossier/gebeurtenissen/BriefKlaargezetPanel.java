package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

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

import java.util.Collections;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.model.TypeGebeurtenis;
import nl.rivm.screenit.main.util.GebeurtenisUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.BriefOpnieuwAanmakenPanel;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.TemplateInzienPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.util.BriefUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

@Slf4j
@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_OPNIEUW_KLAARZETTEN,
		Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Recht.GEBRUIKER_CLIENT_SCREENINGSRONDE },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.MAMMA })
public class BriefKlaargezetPanel extends AbstractGebeurtenisDetailPanel
{
	@SpringBean
	private BaseBriefService baseBriefService;

	private WebMarkupContainer tegenhoudenContainer;

	private final List<BriefType> tegenhoudenNietMogelijkBriefTypes = Collections.singletonList(BriefType.CERVIX_ZAS_UITNODIGING);

	public BriefKlaargezetPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new TemplateInzienPanel("templateInzienPanel", new CompoundPropertyModel(new PropertyModel(getModel(), "brief"))));
		add(new BriefOpnieuwAanmakenPanel("briefOpnieuwAanmakenPanel", new CompoundPropertyModel(new PropertyModel(getModel(), "brief"))));
		tegenhoudenContainer = maakBriefTegenhoudenContent();
		add(tegenhoudenContainer);
	}

	private WebMarkupContainer maakBriefTegenhoudenContent()
	{
		WebMarkupContainer tegenhoudenContainer = new WebMarkupContainer("tegenhoudenContainer");
		tegenhoudenContainer.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Actie.AANPASSEN));

		WebMarkupContainer mogelijk = new WebMarkupContainer("mogelijk");
		ClientBrief<?, ?, ?> brief = getModelObject().getBrief();

		GebeurtenisUtil.voegBriefTypeOfNaamBriefToe(mogelijk, brief);

		mogelijk.add(DateLabel.forDatePattern("brief.creatieDatum", Model.of(brief.getCreatieDatum()), Constants.DEFAULT_DATE_FORMAT));
		boolean tegenhoudenNietMogelijkBevatBriefType = tegenhoudenNietMogelijkBriefTypes.contains(brief.getBriefType());
		mogelijk.add(new Label("nietMeer", Model.of("niet meer"))
			.setVisible(BriefUtil.isGegenereerd(brief) || tegenhoudenNietMogelijkBevatBriefType));
		IndicatingAjaxLink<Void> tegenhoudenLink = new IndicatingAjaxLink<>("tegenhouden")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				baseBriefService.briefTegenhouden(BriefKlaargezetPanel.this.getModelObject().getBrief(), ScreenitSession.get().getLoggedInAccount());
				info(getString("info.brieftegenhouden"));
				verversTegenhouden(target);
			}
		};
		tegenhoudenLink.setVisible(!BriefUtil.isTegengehouden(brief) && !tegenhoudenNietMogelijkBevatBriefType);
		mogelijk.add(tegenhoudenLink);
		mogelijk.add(new IndicatingAjaxLink<Void>("activeren")
		{

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ScreeningRondeGebeurtenis screeningRondeGebeurtenis = BriefKlaargezetPanel.this.getModelObject();
				screeningRondeGebeurtenis.setGebeurtenis(TypeGebeurtenis.BRIEF_TEGENHOUDEN);
				baseBriefService.briefNietMeerTegenhouden(BriefKlaargezetPanel.this.getModelObject().getBrief(), ScreenitSession.get().getLoggedInAccount());
				info(getString("info.briefactiveren"));
				verversTegenhouden(target);
			}
		}.setVisible(BriefUtil.isTegengehouden(brief)));
		mogelijk.setVisible(BriefUtil.isNietGegenereerdEnNietVervangen(brief) && !tegenhoudenNietMogelijkBevatBriefType);
		tegenhoudenContainer.add(mogelijk);

		WebMarkupContainer nietmogelijk = new WebMarkupContainer("nietMogelijk");
		nietmogelijk.add(new Label("tekst", Model.of(getString("message.nietmogelijkbrieftegenhouden"))));

		nietmogelijk.setVisible(BriefUtil.isGegenereerd(brief) || tegenhoudenNietMogelijkBevatBriefType);
		tegenhoudenContainer.add(nietmogelijk);
		tegenhoudenContainer.setOutputMarkupId(true);
		return tegenhoudenContainer;
	}

	private void verversTegenhouden(AjaxRequestTarget target)
	{
		WebMarkupContainer nieuw = maakBriefTegenhoudenContent();
		tegenhoudenContainer.replaceWith(nieuw);
		tegenhoudenContainer = nieuw;
		target.add(tegenhoudenContainer);
	}
}
