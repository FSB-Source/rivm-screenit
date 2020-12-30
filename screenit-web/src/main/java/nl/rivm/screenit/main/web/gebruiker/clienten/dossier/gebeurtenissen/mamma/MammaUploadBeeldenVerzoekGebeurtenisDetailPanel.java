package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenPoging;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseUitwisselportaalService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_MAMMA_INZAGE_UPLOADVERZOEK,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaUploadBeeldenVerzoekGebeurtenisDetailPanel extends AbstractGebeurtenisDetailPanel
{

	@SpringBean
	private BerichtToBatchService berichtToBatchService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaBaseUitwisselportaalService uitwisselportaalService;

	@SpringBean
	private LogService logService;

	public MammaUploadBeeldenVerzoekGebeurtenisDetailPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
		MammaUploadBeeldenVerzoekGebeurtenis uploadBeeldenVerzoekGebeurtenis = (MammaUploadBeeldenVerzoekGebeurtenis) model.getObject();
		MammaUploadBeeldenVerzoek uploadBeeldenVerzoek = uploadBeeldenVerzoekGebeurtenis.getUploadBeeldenVerzoek();
		MammaUploadBeeldenPoging uploadBeeldenPoging = uploadBeeldenVerzoek.getLaatsteUploadPoging();

		add(new EnumLabel<>("uploadBeeldenVerzoek.laatsteUploadPoging.ilmStatus"));
		WebMarkupContainer verzoekContainer = new WebMarkupContainer("verzoek", ModelUtil.csModel(uploadBeeldenVerzoekGebeurtenis.getUploadBeeldenVerzoek()));

		verzoekContainer.add(new Label("ziekenhuis.naam"));
		verzoekContainer.add(new Label("gemaaktDoor.organisatie.naam"));
		verzoekContainer.add(DateLabel.forDatePattern("creatieDatum", "dd-MM-yyyy HH:mm"));
		verzoekContainer.add(new EnumLabel<>("verzoekType"));
		verzoekContainer.add(new EnumLabel<>("status"));

		WebMarkupContainer accessionNumberContainer = new WebMarkupContainer("accessionNumberContainer");
		accessionNumberContainer.add(new Label("laatsteUploadPoging.accessionNumber"));
		accessionNumberContainer.setVisible(uploadBeeldenPoging != null && uploadBeeldenPoging.getAccessionNumber() != null);
		verzoekContainer.add(accessionNumberContainer);

		WebMarkupContainer ingevuldContainer = new WebMarkupContainer("ingevuldContainer");
		ingevuldContainer.add(new EnumLabel<>("conclusieBirads"));
		ingevuldContainer.add(new Label("conclusieEersteUitslagRadiologie"));
		ingevuldContainer.setVisible(uploadBeeldenVerzoek.getConclusieBirads() != null && uploadBeeldenVerzoek.getConclusieEersteUitslagRadiologie() != null);
		ingevuldContainer.setOutputMarkupId(true);
		verzoekContainer.add(ingevuldContainer);

		WebMarkupContainer verwijderenContainer = new WebMarkupContainer("verwijderenContainer");
		IndicatingAjaxLink verwijderButton = new IndicatingAjaxLink<Void>("verwijderen")
		{

			@Override
			public void onClick(AjaxRequestTarget ajaxRequestTarget)
			{
				MammaUploadBeeldenVerzoek uploadBeeldenVerzoek = (MammaUploadBeeldenVerzoek) verzoekContainer.getDefaultModelObject();
				MammaUploadBeeldenPoging uploadBeeldenPoging = uploadBeeldenVerzoek.getLaatsteUploadPoging();
				uitwisselportaalService.verwijderBeelden(uploadBeeldenPoging);
				logService.logGebeurtenis(LogGebeurtenis.MAMMA_UPLOAD_VERZOEK, ScreenitSession.get().getLoggedInInstellingGebruiker(),
					uploadBeeldenVerzoek.getScreeningRonde().getDossier().getClient(),
					"Beelden verwijderd voor uploadverzoek met accessionnummer " + uploadBeeldenPoging.getAccessionNumber(), Bevolkingsonderzoek.MAMMA);
				verwijderenContainer.setVisible(false);
				ingevuldContainer.setVisible(false);
				ajaxRequestTarget.add(verwijderenContainer, ingevuldContainer);

			}
		};
		boolean magVerwijderen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_MAMMA_BEELDEN_VERWIJDEREN_UPLOADVERZOEK, Actie.VERWIJDEREN);
		verwijderenContainer.setVisible(magVerwijderen && uploadBeeldenPoging != null && MammaMammografieIlmStatus.BESCHIKBAAR.equals(uploadBeeldenPoging.getIlmStatus()));
		verwijderenContainer.add(verwijderButton);
		verwijderenContainer.setOutputMarkupId(true);
		add(verwijderenContainer);
		add(verzoekContainer);
	}
}
