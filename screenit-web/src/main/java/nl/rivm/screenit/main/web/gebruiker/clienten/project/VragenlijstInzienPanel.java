package nl.rivm.screenit.main.web.gebruiker.clienten.project;

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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.AbstractGebeurtenisDetailPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.GebeurtenisPopupBasePanel;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.FormulierRenderContext;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.formulieren.ScreenitFormulierRenderPanel;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.project.ProjectBrief;
import nl.topicuszorg.formulieren2.persistence.resultaat.FormulierResultaatImpl;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_PROJECT_VRAGENLIJST_ANTWOORDEN_INZIEN },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.CERVIX })
public class VragenlijstInzienPanel extends AbstractGebeurtenisDetailPanel
{

	private static final long serialVersionUID = 1L;

	private Panel vragenlijst;

	public VragenlijstInzienPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);

		vragenlijst = getVragenlijstPanel();
		vragenlijst.setOutputMarkupId(true);
		add(vragenlijst);
	}

	private ScreenitFormulierRenderPanel getVragenlijstPanel()
	{
		ScreenitFormulierInstantie formulierInstantie = ((ProjectBrief) getModelObject().getBrief()).getVragenlijstAntwoordenHolder().getVragenlijstAntwoorden()
			.getFormulierInstantie();
		ScreenitFormulierRenderPanel vragenlijst = new ScreenitFormulierRenderPanel("vragenlijst", ModelUtil.sModel(formulierInstantie))
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected IModel<FormulierResultaatImpl> createRenderContextModel(ScreenitFormulierInstantie formulierInstantie, FormulierRenderContext formulierRenderContext)
			{
				PropertyModel<FormulierResultaatImpl> formulierResultaatModel = new PropertyModel<FormulierResultaatImpl>(VragenlijstInzienPanel.this.getDefaultModel(),
					"brief.vragenlijstAntwoordenHolder.vragenlijstAntwoorden.resultaat");
				return formulierResultaatModel;
			}
		};
		vragenlijst.setEnabled(false);
		return vragenlijst;
	}

	private VragenlijstScanPanel getScanPanel()
	{
		return new VragenlijstScanPanel("vragenlijst", this.getModel());
	}

	@Override
	protected void addButton(String id, GebeurtenisPopupBasePanel parent)
	{
		AjaxLink<ScreeningRondeGebeurtenis> button = new AjaxLink<ScreeningRondeGebeurtenis>(id, VragenlijstInzienPanel.this.getModel())
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				if (VragenlijstInzienPanel.this.vragenlijst instanceof ScreenitFormulierRenderPanel)
				{
					VragenlijstScanPanel vragenlijst = getScanPanel();
					VragenlijstInzienPanel.this.vragenlijst.replaceWith(vragenlijst);
					VragenlijstInzienPanel.this.vragenlijst = vragenlijst;
					target.add(VragenlijstInzienPanel.this.vragenlijst);
				}
				else
				{
					ScreenitFormulierRenderPanel vragenlijst = getVragenlijstPanel();
					VragenlijstInzienPanel.this.vragenlijst.replaceWith(vragenlijst);
					VragenlijstInzienPanel.this.vragenlijst = vragenlijst;
					target.add(VragenlijstInzienPanel.this.vragenlijst);
				}
			}
		};
		button.add(new Label("label", getString("label.papierdigitaal")));
		button.setVisible(((ProjectBrief) VragenlijstInzienPanel.this.getModelObject().getBrief()).getVragenlijstAntwoordenHolder().getScannedVragenlijst() != null);
		parent.add(button);
	}
}
