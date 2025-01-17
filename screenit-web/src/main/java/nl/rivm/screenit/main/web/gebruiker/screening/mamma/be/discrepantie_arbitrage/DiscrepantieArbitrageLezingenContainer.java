package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.discrepantie_arbitrage;

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

import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.AbstractBEAccordionPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaLezingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaLezingParameters;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaBeLezerSoort;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaLezingType;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class DiscrepantieArbitrageLezingenContainer extends AbstractBEAccordionPanel<MammaBeoordeling>
{
	private final MammaDiscrepantieArbitrageRondePanel parent;

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	private IModel<MammaLezing> huidigeLezingModel;

	private MammaLezingPanel huidigeLezing;

	DiscrepantieArbitrageLezingenContainer(String id, IModel<MammaBeoordeling> model, MammaDiscrepantieArbitrageRondePanel parent)
	{
		super(id, model, Model.of("BI-RADS beoordeling"), 12);
		this.parent = parent;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		IModel<MammaLezing> discrepantieLezing = new CompoundPropertyModel<>(new PropertyModel<>(getModel(), "discrepantieLezing"));

		IModel<MammaLezing> eersteLezing = new CompoundPropertyModel<>(new PropertyModel<>(getModel(), "eersteLezing"));
		IModel<MammaLezing> tweedeLezing = new CompoundPropertyModel<>(new PropertyModel<>(getModel(), "tweedeLezing"));

		InstellingGebruiker gebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		huidigeLezingModel = ModelUtil.cModel(beoordelingService.getOrCreateDiscrepantieOfArbitrageLezing(getModelObject(), huidigeLezingType(), gebruiker));

		MammaLezingParameters mammaLezingParameters = MammaLezingParameters.maakAlleenInzien()
			.setMetAfbeelding(true)
			.setToonOvernemenKnop(!isDezeBeoordelingAlleenInzien())
			.setToonTomosyntheseSlicesRadioButtons(MammaOnderzoekType.TOMOSYNTHESE == getModelObject().getOnderzoek().getOnderzoekType())
			.setAmputatie(getModelObject().getOnderzoek().getAmputatie());
		panelContainer.add(new MammaLezingPanel(this, "eersteLezing", eersteLezing, mammaLezingParameters));
		panelContainer.add(new MammaLezingPanel(this, "tweedeLezing", tweedeLezing, mammaLezingParameters));
		toonDiscrepantieopmerkingIndienVanToepassing(discrepantieLezing, eersteLezing);

		huidigeLezing = new MammaLezingPanel(this, "huidigeLezing", huidigeLezingModel, parametersHuidigeLezing());
		huidigeLezing.setOutputMarkupId(true);
		panelContainer.add(huidigeLezing);
		panelContainer.setOutputMarkupId(true);
	}

	private MammaLezingType huidigeLezingType()
	{
		return parent.getLezerSoort() == MammaBeLezerSoort.DISCREPANTIE_LEZER ? MammaLezingType.DISCREPANTIE_LEZING : MammaLezingType.ARBITRAGE_LEZING;
	}

	private MammaLezingParameters parametersHuidigeLezing()
	{
		var onderzoek = getModelObject().getOnderzoek();
		return new MammaLezingParameters()
			.setInzien(isDezeBeoordelingAlleenInzien())
			.setMetAfbeelding(true)
			.setAmputatie(onderzoek.getAmputatie())
			.setVerwijzenRechtsVerplicht(baseBeoordelingService.isDiscrepantieVerwijzingVerplicht(getModelObject(), MammaZijde.RECHTER_BORST))
			.setVerwijzenLinksVerplicht(baseBeoordelingService.isDiscrepantieVerwijzingVerplicht(getModelObject(), MammaZijde.LINKER_BORST))
			.setToonBiradsOpmerkingVeld(true)
			.setToonTomosyntheseSlicesRadioButtons(MammaOnderzoekType.TOMOSYNTHESE == onderzoek.getOnderzoekType());
	}

	private void toonDiscrepantieopmerkingIndienVanToepassing(IModel<MammaLezing> discrepantieLezing, IModel<MammaLezing> eersteLezing)
	{
		if (huidigeLezingType() == MammaLezingType.ARBITRAGE_LEZING)
		{

			boolean doorEersteBeoordelaar = eersteLezing.getObject().getBeoordelaar().equals(discrepantieLezing.getObject().getBeoordelaar());
			panelContainer.add(
				new DiscrepantieOpmerkingPanel(doorEersteBeoordelaar ? "discrepantieopmerkingOnderEersteLezing" : "discrepantieopmerkingOnderTweedeLezing", discrepantieLezing));
			panelContainer.add(new EmptyPanel(doorEersteBeoordelaar ? "discrepantieopmerkingOnderTweedeLezing" : "discrepantieopmerkingOnderEersteLezing"));
		}
		else
		{
			panelContainer.add(new EmptyPanel("discrepantieopmerkingOnderTweedeLezing"));
			panelContainer.add(new EmptyPanel("discrepantieopmerkingOnderEersteLezing"));
		}
	}

	public IModel<MammaLezing> getHuidigeLezingModel()
	{
		return huidigeLezingModel;
	}

	private boolean isDezeBeoordelingAlleenInzien()
	{
		return MammaBeoordelingStatus.VERSLAG_MAKEN.equals(getModelObject().getStatus()) || MammaBeoordelingStatus.UITSLAG_GUNSTIG.equals(getModelObject().getStatus())
			|| (MammaBeoordelingStatus.ARBITRAGE.equals(getModelObject().getStatus()) && beoordeeldDoorIngelogdeGebruiker());
	}

	private boolean beoordeeldDoorIngelogdeGebruiker()
	{
		InstellingGebruiker instellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		return instellingGebruiker.equals(getModelObject().getEersteLezing().getBeoordelaar())
			|| instellingGebruiker.equals(getModelObject().getTweedeLezing().getBeoordelaar());
	}

	public void beoordelingNaarArbitrageEnOpslaan(MammaLezing lezing, AjaxRequestTarget target)
	{
		parent.beoordelingNaarArbitrageEnOpslaan(lezing, target);
	}

	public void lezingOpslaan(MammaLezing lezing, AjaxRequestTarget target, List<LaesieDto> lezingToLaesieDtos)
	{
		parent.lezingOpslaan(lezing, target, lezingToLaesieDtos);
	}

	public void blokkeerLezing(AjaxRequestTarget target)
	{
		huidigeLezing.blokeerOpslaan(target);
	}
}
